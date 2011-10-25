#include <assert.h>
#include <math.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG

#include "debug.h"
#include "board.h"
#include "move.h"
#include "playout.h"
#include "probdist.h"
#include "random.h"
#include "tactics/util.h"
#include "uct/dynkomi.h"
#include "uct/internal.h"
#include "uct/search.h"
#include "uct/tree.h"
#include "uct/uct.h"
#include "uct/walk.h"

#define DESCENT_DLEN 512

void
uct_progress_status(struct uct *u, struct tree *t, enum stone color, int playouts)
{
	if (!UDEBUGL(0))
		return;

	/* Best move */
	struct tree_node *best = u->policy->choose(u->policy, t->root, t->board, color, resign);
	if (!best) {
		fprintf(stderr, "... No moves left\n");
		return;
	}
	fprintf(stderr, "[%d] ", playouts);
	fprintf(stderr, "best %f ", tree_node_get_value(t, 1, best->u.value));

	/* Dynamic komi */
	if (t->use_extra_komi)
		fprintf(stderr, "komi %.1f ", t->extra_komi);

	/* Best sequence */
	fprintf(stderr, "| seq ");
	for (int depth = 0; depth < 4; depth++) {
		if (best && best->u.playouts >= 25) {
			fprintf(stderr, "%3s ", coord2sstr(node_coord(best), t->board));
			best = u->policy->choose(u->policy, best, t->board, color, resign);
		} else {
			fprintf(stderr, "    ");
		}
	}

	/* Best candidates */
	fprintf(stderr, "| can ");
	int cans = 4;
	struct tree_node *can[cans];
	memset(can, 0, sizeof(can));
	best = t->root->children;
	while (best) {
		int c = 0;
		while ((!can[c] || best->u.playouts > can[c]->u.playouts) && ++c < cans);
		for (int d = 0; d < c; d++) can[d] = can[d + 1];
		if (c > 0) can[c - 1] = best;
		best = best->sibling;
	}
	while (--cans >= 0) {
		if (can[cans]) {
			fprintf(stderr, "%3s(%.3f) ",
			        coord2sstr(node_coord(can[cans]), t->board),
				tree_node_get_value(t, 1, can[cans]->u.value));
		} else {
			fprintf(stderr, "           ");
		}
	}

	fprintf(stderr, "\n");
}


static inline void
record_amaf_move(struct playout_amafmap *amaf, coord_t coord)
{
	assert(amaf->gamelen < MAX_GAMELEN);
	amaf->game[amaf->gamelen++] = coord;
}


struct uct_playout_callback {
	struct uct *uct;
	struct tree *tree;
	struct tree_node *lnode;
};


static coord_t
uct_playout_hook(struct playout_policy *playout, struct playout_setup *setup, struct board *b, enum stone color, int mode)
{
	/* XXX: This is used in some non-master branches. */
	return pass;
}

static coord_t
uct_playout_prepolicy(struct playout_policy *playout, struct playout_setup *setup, struct board *b, enum stone color)
{
	return uct_playout_hook(playout, setup, b, color, 0);
}

static coord_t
uct_playout_postpolicy(struct playout_policy *playout, struct playout_setup *setup, struct board *b, enum stone color)
{
	return uct_playout_hook(playout, setup, b, color, 1);
}


static int
uct_leaf_node(struct uct *u, struct board *b, enum stone player_color,
              struct playout_amafmap *amaf,
	      struct uct_descent *descent, int *dlen,
	      struct tree_node *significant[2],
              struct tree *t, struct tree_node *n, enum stone node_color,
	      char *spaces)
{
	enum stone next_color = stone_other(node_color);
	int parity = (next_color == player_color ? 1 : -1);

	if (UDEBUGL(7))
		fprintf(stderr, "%s*-- UCT playout #%d start [%s] %f\n",
			spaces, n->u.playouts, coord2sstr(node_coord(n), t->board),
			tree_node_get_value(t, parity, n->u.value));

	struct uct_playout_callback upc = {
		.uct = u,
		.tree = t,
		/* TODO: Don't necessarily restart the sequence walk when
		 * entering playout. */
		.lnode = NULL,
	};

	struct playout_setup ps = {
		.gamelen = u->gamelen,
		.mercymin = u->mercymin,
		.prepolicy_hook = uct_playout_prepolicy,
		.postpolicy_hook = uct_playout_postpolicy,
		.hook_data = &upc,
	};
	int result = play_random_game(&ps, b, next_color,
	                              u->playout_amaf ? amaf : NULL,
				      &u->ownermap, u->playout);
	if (next_color == S_WHITE) {
		/* We need the result from black's perspective. */
		result = - result;
	}
	if (UDEBUGL(7))
		fprintf(stderr, "%s -- [%d..%d] %s random playout result %d\n",
		        spaces, player_color, next_color, coord2sstr(node_coord(n), t->board), result);

	return result;
}

static floating_t
scale_value(struct uct *u, struct board *b, int result)
{
	floating_t rval = result > 0 ? 1.0 : result < 0 ? 0.0 : 0.5;
	if (u->val_scale && result != 0) {
		int vp = u->val_points;
		if (!vp) {
			vp = board_size(b) - 1; vp *= vp; vp *= 2;
		}

		floating_t sval = (floating_t) abs(result) / vp;
		sval = sval > 1 ? 1 : sval;
		if (result < 0) sval = 1 - sval;
		if (u->val_extra)
			rval += u->val_scale * sval;
		else
			rval = (1 - u->val_scale) * rval + u->val_scale * sval;
		// fprintf(stderr, "score %d => sval %f, rval %f\n", result, sval, rval);
	}
	return rval;
}

static double
local_value(struct uct *u, struct board *b, coord_t coord, enum stone color)
{
	/* Tactical evaluation of move @coord by color @color, given
	 * simulation end position @b. I.e., a move is tactically good
	 * if the resulting group stays on board until the game end. */
	/* We can also take into account surrounding stones, e.g. to
	 * encourage taking off external liberties during a semeai. */
	double val = board_local_value(u->local_tree_neival, b, coord, color);
	return (color == S_WHITE) ? 1.f - val : val;
}

static void
record_local_sequence(struct uct *u, struct tree *t, struct board *endb,
                      struct uct_descent *descent, int dlen, int di,
		      enum stone seq_color)
{
#define LTREE_DEBUG if (UDEBUGL(6))

	/* Ignore pass sequences. */
	if (is_pass(node_coord(descent[di].node)))
		return;

	LTREE_DEBUG board_print(endb, stderr);
	LTREE_DEBUG fprintf(stderr, "recording local %s sequence: ",
		stone2str(seq_color));

	/* Sequences starting deeper are less relevant in general. */
	int pval = LTREE_PLAYOUTS_MULTIPLIER;
	if (u->local_tree && u->local_tree_depth_decay > 0)
		pval = ((floating_t) pval) / pow(u->local_tree_depth_decay, di - 1);
	if (!pval) {
		LTREE_DEBUG fprintf(stderr, "too deep @%d\n", di);
		return;
	}

	/* Pick the right local tree root... */
	struct tree_node *lnode = seq_color == S_BLACK ? t->ltree_black : t->ltree_white;
	lnode->u.playouts++;

	double sval = 0.5;
	if (u->local_tree_rootgoal) {
		sval = local_value(u, endb, node_coord(descent[di].node), seq_color);
		LTREE_DEBUG fprintf(stderr, "(goal %s[%s %1.3f][%d]) ",
			coord2sstr(node_coord(descent[di].node), t->board),
			stone2str(seq_color), sval, descent[di].node->d);
	}

	/* ...and record the sequence. */
	int di0 = di;
	while (di < dlen && (di == di0 || descent[di].node->d < u->tenuki_d)) {
		enum stone color = (di - di0) % 2 ? stone_other(seq_color) : seq_color;
		double rval;
		if (u->local_tree_rootgoal)
			rval = sval;
		else
			rval = local_value(u, endb, node_coord(descent[di].node), color);
		LTREE_DEBUG fprintf(stderr, "%s[%s %1.3f][%d] ",
			coord2sstr(node_coord(descent[di].node), t->board),
			stone2str(color), rval, descent[di].node->d);
		lnode = tree_get_node(t, lnode, node_coord(descent[di++].node), true);
		assert(lnode);
		stats_add_result(&lnode->u, rval, pval);
	}

	/* Add lnode for tenuki (pass) if we descended further. */
	if (di < dlen) {
		double rval = u->local_tree_rootgoal ? sval : 0.5;
		LTREE_DEBUG fprintf(stderr, "pass ");
		lnode = tree_get_node(t, lnode, pass, true);
		assert(lnode);
		stats_add_result(&lnode->u, rval, pval);
	}
	
	LTREE_DEBUG fprintf(stderr, "\n");
}


int
uct_playout(struct uct *u, struct board *b, enum stone player_color, struct tree *t)
{
	struct board b2;
	board_copy(&b2, b);

	struct playout_amafmap amaf;
	amaf.gamelen = amaf.game_baselen = 0;

	/* Walk the tree until we find a leaf, then expand it and do
	 * a random playout. */
	struct tree_node *n = t->root;
	enum stone node_color = stone_other(player_color);
	assert(node_color == t->root_color);

	/* Make sure the root node is expanded. */
	if (tree_leaf_node(n) && !__sync_lock_test_and_set(&n->is_expanded, 1))
		tree_expand_node(t, n, &b2, player_color, u, 1);

	/* Tree descent history. */
	/* XXX: This is somewhat messy since @n and descent[dlen-1].node are
	 * redundant. */
	struct uct_descent descent[DESCENT_DLEN];
	descent[0].node = n; descent[0].lnode = NULL;
	int dlen = 1;
	/* Total value of the sequence. */
	struct move_stats seq_value = { .playouts = 0 };
	/* The last "significant" node along the descent (i.e. node
	 * with higher than configured number of playouts). For black
	 * and white. */
	struct tree_node *significant[2] = { NULL, NULL };
	if (n->u.playouts >= u->significant_threshold)
		significant[node_color - 1] = n;

	int result;
	int pass_limit = (board_size(&b2) - 2) * (board_size(&b2) - 2) / 2;
	int passes = is_pass(b->last_move.coord) && b->moves > 0;

	/* debug */
	static char spaces[] = "\0                                                      ";
	/* /debug */
	if (UDEBUGL(8))
		fprintf(stderr, "--- UCT walk with color %d\n", player_color);

	while (!tree_leaf_node(n) && passes < 2) {
		spaces[dlen - 1] = ' '; spaces[dlen] = 0;


		/*** Choose a node to descend to: */

		/* Parity is chosen already according to the child color, since
		 * it is applied to children. */
		node_color = stone_other(node_color);
		int parity = (node_color == player_color ? 1 : -1);

		assert(dlen < DESCENT_DLEN);
		descent[dlen] = descent[dlen - 1];
		if (u->local_tree && (!descent[dlen].lnode || descent[dlen].node->d >= u->tenuki_d)) {
			/* Start new local sequence. */
			/* Remember that node_color already holds color of the
			 * to-be-found child. */
			descent[dlen].lnode = node_color == S_BLACK ? t->ltree_black : t->ltree_white;
		}

		if (!u->random_policy_chance || fast_random(u->random_policy_chance))
			u->policy->descend(u->policy, t, &descent[dlen], parity, b2.moves > pass_limit);
		else
			u->random_policy->descend(u->random_policy, t, &descent[dlen], parity, b2.moves > pass_limit);


		/*** Perform the descent: */

		if (descent[dlen].node->u.playouts >= u->significant_threshold) {
			significant[node_color - 1] = descent[dlen].node;
		}

		seq_value.playouts += descent[dlen].value.playouts;
		seq_value.value += descent[dlen].value.value * descent[dlen].value.playouts;
		n = descent[dlen++].node;
		assert(n == t->root || n->parent);
		if (UDEBUGL(7))
			fprintf(stderr, "%s+-- UCT sent us to [%s:%d] %d,%f\n",
			        spaces, coord2sstr(node_coord(n), t->board),
				node_coord(n), n->u.playouts,
				tree_node_get_value(t, parity, n->u.value));

		/* Add virtual loss if we need to; this is used to discourage
		 * other threads from visiting this node in case of multiple
		 * threads doing the tree search. */
		if (u->virtual_loss)
			stats_add_result(&n->u, node_color == S_BLACK ? 0.0 : 1.0, u->virtual_loss);

		assert(node_coord(n) >= -1);
		record_amaf_move(&amaf, node_coord(n));

		struct move m = { node_coord(n), node_color };
		int res = board_play(&b2, &m);

		if (res < 0 || (!is_pass(m.coord) && !group_at(&b2, m.coord)) /* suicide */
		    || b2.superko_violation) {
			if (UDEBUGL(4)) {
				for (struct tree_node *ni = n; ni; ni = ni->parent)
					fprintf(stderr, "%s<%"PRIhash"> ", coord2sstr(node_coord(ni), t->board), ni->hash);
				fprintf(stderr, "marking invalid %s node %d,%d res %d group %d spk %d\n",
				        stone2str(node_color), coord_x(node_coord(n),b), coord_y(node_coord(n),b),
					res, group_at(&b2, m.coord), b2.superko_violation);
			}
			n->hints |= TREE_HINT_INVALID;
			result = 0;
			goto end;
		}

		if (is_pass(node_coord(n)))
			passes++;
		else
			passes = 0;

		enum stone next_color = stone_other(node_color);
		/* We need to make sure only one thread expands the node. If
		 * we are unlucky enough for two threads to meet in the same
		 * node, the latter one will simply do another simulation from
		 * the node itself, no big deal. t->nodes_size may exceed
		 * the maximum in multi-threaded case but not by much so it's ok.
		 * The size test must be before the test&set not after, to allow
		 * expansion of the node later if enough nodes have been freed. */
		if (tree_leaf_node(n)
		    && n->u.playouts - u->virtual_loss >= u->expand_p && t->nodes_size < u->max_tree_size
		    && !__sync_lock_test_and_set(&n->is_expanded, 1))
			tree_expand_node(t, n, &b2, next_color, u, -parity);
	}

	amaf.game_baselen = amaf.gamelen;

	if (t->use_extra_komi && u->dynkomi->persim) {
		b2.komi += round(u->dynkomi->persim(u->dynkomi, &b2, t, n));
	}

	if (passes >= 2) {
		/* XXX: No dead groups support. */
		floating_t score = board_official_score(&b2, NULL);
		/* Result from black's perspective (no matter who
		 * the player; black's perspective is always
		 * what the tree stores. */
		result = - (score * 2);

		if (UDEBUGL(5))
			fprintf(stderr, "[%d..%d] %s p-p scoring playout result %d (W %f)\n",
				player_color, node_color, coord2sstr(node_coord(n), t->board), result, score);
		if (UDEBUGL(6))
			board_print(&b2, stderr);

		board_ownermap_fill(&u->ownermap, &b2);

	} else { // assert(tree_leaf_node(n));
		/* In case of parallel tree search, the assertion might
		 * not hold if two threads chew on the same node. */
		result = uct_leaf_node(u, &b2, player_color, &amaf, descent, &dlen, significant, t, n, node_color, spaces);
	}

	if (u->policy->wants_amaf && u->playout_amaf_cutoff) {
		unsigned int cutoff = amaf.game_baselen;
		cutoff += (amaf.gamelen - amaf.game_baselen) * u->playout_amaf_cutoff / 100;
		amaf.gamelen = cutoff;
	}

	/* Record the result. */

	assert(n == t->root || n->parent);
	floating_t rval = scale_value(u, b, result);
	u->policy->update(u->policy, t, n, node_color, player_color, &amaf, &b2, rval);

	if (t->use_extra_komi) {
		stats_add_result(&u->dynkomi->score, result / 2, 1);
		stats_add_result(&u->dynkomi->value, rval, 1);
	}

	if (u->local_tree && n->parent && !is_pass(node_coord(n)) && dlen > 0) {
		/* Get the local sequences and record them in ltree. */
		/* We will look for sequence starts in our descent
		 * history, then run record_local_sequence() for each
		 * found sequence start; record_local_sequence() may
		 * pick longer sequences from descent history then,
		 * which is expected as it will create new lnodes. */
		enum stone seq_color = player_color;
		/* First move always starts a sequence. */
		record_local_sequence(u, t, &b2, descent, dlen, 1, seq_color);
		seq_color = stone_other(seq_color);
		for (int dseqi = 2; dseqi < dlen; dseqi++, seq_color = stone_other(seq_color)) {
			if (u->local_tree_allseq) {
				/* We are configured to record all subsequences. */
				record_local_sequence(u, t, &b2, descent, dlen, dseqi, seq_color);
				continue;
			}
			if (descent[dseqi].node->d >= u->tenuki_d) {
				/* Tenuki! Record the fresh sequence. */
				record_local_sequence(u, t, &b2, descent, dlen, dseqi, seq_color);
				continue;
			}
			if (descent[dseqi].lnode && !descent[dseqi].lnode) {
				/* Record result for in-descent picked sequence. */
				record_local_sequence(u, t, &b2, descent, dlen, dseqi, seq_color);
				continue;
			}
		}
	}

end:
	/* We need to undo the virtual loss we added during descend. */
	if (u->virtual_loss) {
		floating_t loss = node_color == S_BLACK ? 0.0 : 1.0;
		for (; n->parent; n = n->parent) {
			stats_rm_result(&n->u, loss, u->virtual_loss);
			loss = 1.0 - loss;
		}
	}

	board_done_noalloc(&b2);
	return result;
}

int
uct_playouts(struct uct *u, struct board *b, enum stone color, struct tree *t, struct time_info *ti)
{
	int i;
	if (ti && ti->dim == TD_GAMES) {
		for (i = 0; t->root->u.playouts <= ti->len.games && !uct_halt; i++)
			uct_playout(u, b, color, t);
	} else {
		for (i = 0; !uct_halt; i++)
			uct_playout(u, b, color, t);
	}
	return i;
}
