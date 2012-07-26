
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<math.h>

#include<iostream>
#include<algorithm>

const int MaxSize = 15;
const int MaxProblem = 20;

class Move {
 public:
  int nbLocations;
  int locations [MaxSize * MaxSize];

  Move () {
    nbLocations = 0;
  }

  void add (int loc) {
    locations [nbLocations] = loc;
    nbLocations++;
  }

  void load (FILE *fp) {
    fscanf (fp, "%d", &nbLocations);
    for (int i = 0; i < nbLocations; i++)
      fscanf (fp, "%d", &locations [i]);
  }

  void print (FILE *fp) {
    fprintf (fp, "%d ", nbLocations);
    for (int i = 0; i < nbLocations; i++)
      fprintf (fp, "%d ", locations [i]);
    fprintf (fp, "\n");
  }

  void printCoord (FILE *fp) {
    fprintf (fp, "(%d,%d) ", locations [0] % MaxSize, locations [0] / MaxSize);
  }

  void sort () {
    //print (stderr);
    std::sort (locations, locations + nbLocations);
    //print (stderr);
  }
};

class Seen {
 public:
  char seen [MaxSize * MaxSize];

  void init () {
    memset (seen, 0, MaxSize * MaxSize * sizeof (char));
  }

  bool test (int loc) {
    return (seen [loc] == 0);
  }

  void set (int loc) {
    seen [loc] = 1;
  }
};

class HighScore {
 public:
  int score;
  int lengthVariation;
  Move variation [MaxSize * MaxSize];

  void load (FILE *fp) {
    fscanf (fp, "%d", &score);
    fscanf (fp, "%d", &lengthVariation);
    for (int i = 0; i < lengthVariation; i++)
      variation [i].load (fp);
  }

  void print (FILE *fp) {
    fprintf (fp, "%d\n", score);
    fprintf (fp, "%d\n", lengthVariation);
    for (int i = 0; i < lengthVariation; i++)
      variation [i].print (fp);
  }
};

static Move moves [MaxSize * MaxSize];

class Problem {
 public:
  int color [MaxSize * MaxSize];
  int nbMoves;
  int score;
  int lengthVariation;
  Move variation [MaxSize * MaxSize];


  void load (FILE *fp) {
    for (int i = 0; i < MaxSize; i++)
      for (int j = 0; j < MaxSize; j++)
	fscanf (fp, "%d", &color [MaxSize * i + j]);
    score = 0;
    lengthVariation = 0;
  }
  
  void print (FILE *fp) {
    for (int i = 0; i < MaxSize; i++) {
      for (int j = 0; j < MaxSize; j++)
	fprintf (fp, "%d ", color [MaxSize * i + j]);
      fprintf (fp, "\n");
    }
    fprintf (fp, "score = %d\n", score);
  }

  void buildMove (int loc, Seen & seen, Move & move) {
    int c = color [loc];
    seen.set (loc);
    move.locations [0] = loc;
    move.nbLocations = 1;
    int stack [MaxSize * MaxSize];
    stack [0] = 1;
    stack [1] = loc;
    while (stack [0] > 0) {
      int l = stack [stack [0]], neigh;
      stack [0]--;
      if (l >= MaxSize) {
	neigh = l - MaxSize;
	if (color [neigh] == c)
	  if (seen.test (neigh)) {
	    seen.set (neigh);
	    move.add (neigh);
	    stack [0]++;
	    stack [stack [0]] = neigh;
	  }
      }
      if (l < MaxSize * MaxSize - MaxSize) {
	neigh = l + MaxSize;
	if (color [neigh] == c)
	  if (seen.test (neigh)) {
	    seen.set (neigh);
	    move.add (neigh);
	    stack [0]++;
	    stack [stack [0]] = neigh;
	  }
      }
      if ((l % MaxSize) != 0) {
	neigh = l - 1;
	if (color [neigh] == c)
	  if (seen.test (neigh)) {
	    seen.set (neigh);
	    move.add (neigh);
	    stack [0]++;
	    stack [stack [0]] = neigh;
	  }
      }
      if ((l % MaxSize) != MaxSize - 1) {
	neigh = l + 1;
	if (color [neigh] == c)
	  if (seen.test (neigh)) {
	    seen.set (neigh);
	    move.add (neigh);
	    stack [0]++;
	    stack [stack [0]] = neigh;
	  }
      }
    }
  }

  bool moreThanOneMove (int c) {
    Seen seen;
    Move mv;
    int nb = 0;
    seen.init ();
    for (int i = 0; i < MaxSize * MaxSize; i++) 
      if (color [i] == c)
	if (seen.test (i)) {
	  buildMove (i, seen, mv);
	  nb++;
	  if (nb > 1)
	    return true;
	}
    return false;
  }
  
  void findMoves (int tabu = 9, int secondBest = 9) {
    Seen seen;
    nbMoves = 0;
    seen.init ();
    if (!moreThanOneMove (tabu))
      tabu = 9;
    if ((!moreThanOneMove (secondBest)) || ((rand () % 10) > 9))
      secondBest = 9;
    for (int i = 0; i < MaxSize * MaxSize; i++) 
      if ((color [i] != 9) && (color [i] != tabu) && (color [i] != secondBest))
	if (seen.test (i)) {
	  buildMove (i, seen, moves [nbMoves]);
	  if (moves [nbMoves].nbLocations > 1)
	    nbMoves++;
	}

    if (nbMoves == 0) {
      for (int i = 0; i < MaxSize * MaxSize; i++) 
	if ((color [i] != 9) && (color [i] != tabu))
	  if (seen.test (i)) {
	    buildMove (i, seen, moves [nbMoves]);
	    if (moves [nbMoves].nbLocations > 1)
	      nbMoves++;
	  } 
    }

    if (nbMoves == 0) {
      for (int i = 0; i < MaxSize * MaxSize; i++) 
	if (color [i] != 9)
	  if (seen.test (i)) {
	    buildMove (i, seen, moves [nbMoves]);
	    if (moves [nbMoves].nbLocations > 1)
	      nbMoves++;
	  } 
    }
/*     for (int i = 0; i < nbMoves; i++) */
/*       moves [i].print (stderr); */
  }
  
  void findMovesColor (int c) {
    Seen seen;
    nbMoves = 0;
    seen.init ();
    for (int i = 0; i < MaxSize * MaxSize; i++) 
      if (color [i] == c)
	if (seen.test (i)) {
	  buildMove (i, seen, moves [nbMoves]);
	  if (moves [nbMoves].nbLocations > 1)
	    nbMoves++;
	}

    if (nbMoves == 0) {
      for (int i = 0; i < MaxSize * MaxSize; i++) 
	if (color [i] != 9)
	  if (seen.test (i)) {
	    buildMove (i, seen, moves [nbMoves]);
	    if (moves [nbMoves].nbLocations > 1)
	      nbMoves++;
	  } 
    }
  }
  
  void remove (int loc) {
    while (loc > MaxSize - 1) {
      color [loc] = color [loc - MaxSize];
      loc = loc - MaxSize;
    }
    color [loc] = 9;
  }

  void removeColumn (int column) {
    for (int row = 0; row < MaxSize; row++) {
      for (int i = column; i < MaxSize - 1; i++) {
	color [row * MaxSize + i] = color [row * MaxSize + i + 1];
      }
      color [row * MaxSize + MaxSize - 1] = 9;
    }
  }

  void playMove (Move & move) {
    // it is necessary to sort in order to remove
    // location on top before
    move.sort ();
    for (int i = 0; i < move.nbLocations; i++) {
      remove (move.locations [i]);
    }

    int column = 0;
    for (int i = 0; i < MaxSize; i++) {
      if (color [MaxSize * MaxSize - MaxSize + column] == 9)
	removeColumn (column);
      else
	column++;
    }

    score += (move.nbLocations - 2) * (move.nbLocations - 2);

    if (color [MaxSize * MaxSize - MaxSize] == 9)
      score += 1000;

    variation [lengthVariation] = move;
    lengthVariation++;
  }

  int bestColor (int & secondBest) {
    int nbColors [10];
    for (int i = 0; i < 10; i++)
      nbColors [i] = 0;
    for (int i = 0; i < MaxSize * MaxSize; i++) 
      if (color [i] != 9)
	nbColors [color [i]]++;
    int best = 0, bestScore = 0, secondBestScore = 0;
    for (int i = 0; i < 10; i++)
      if (nbColors [i] > bestScore) {
	secondBestScore = bestScore;
	secondBest = best;
	bestScore = nbColors [i];
	best = i;
      }
      else if (nbColors [i] > secondBestScore) {
	secondBestScore = nbColors [i];
	secondBest = i;
      }
    return best;
  }

  void playout () {
    int secondBest = 9;
    int tabu = bestColor (secondBest);
    findMoves (tabu, secondBest);
    while (nbMoves > 0) {
      int index = nbMoves * (rand () / (RAND_MAX + 1.0));
      playMove (moves [index]);
      findMoves (tabu, secondBest);
    }
  }

  int evaluation () {
    findMoves ();
    int score = 0;
    for (int i = 0; i < nbMoves; i++) {
      score += (moves [i].nbLocations - 2) * (moves [i].nbLocations - 2);
    }
    return score;
  }


  int chooseColor (int tabu = 9) {
    int nbColors [10], maxColor = 5, total = 0;
    for (int i = 0; i < 10; i++)
      nbColors [i] = 0;
    for (int i = 0; i < MaxSize * MaxSize; i++) 
      if (color [i] != 9) {
	nbColors [color [i]]++;
	total++;
/* 	if (color [i] > maxColor) */
/* 	  maxColor = color [i]; */
      }
    float beta = 4.0;
    float alpha = 1.0 + beta * total / 225.0;
    float theta = 0.0;
    int best = 0, minScore = nbColors [0];
    for (int i = 1; i < maxColor; i++)
      if (nbColors [i] < minScore) {
	minScore = nbColors [i];
	best = i;
      }
    theta = minScore / 2.0;
    float term [10], sumTerms = 0.0;
    for (int i = 0; i < maxColor; i++) {
      term [i] = pow (nbColors [i] - theta, alpha);
      if (i == tabu)
	term [i] = 0.0;
      sumTerms += term [i];
    }
    float proba [10], sumProbas = 0.0;
    for (int i = 0; i < maxColor; i++) {
      proba [i] = (1.0 - (term [i] / sumTerms)) / (maxColor - 1);
      if (i == tabu)
	proba [i] = 0.0;
      sumProbas += proba [i];
    }
    float r = sumProbas * (rand () / (RAND_MAX + 1.0));
    float s = 0.0;
    for (int i = 0; i < maxColor; i++) {
      s += proba [i];
      if (s > r)
	return i;
    }
    return tabu;
  }

  void playoutColored () {
    int secondBest = 9;
    int tabu = bestColor (secondBest);
    int color = chooseColor (tabu);
    findMovesColor (color);
    while (nbMoves > 0) {
      int index = nbMoves * (rand () / (RAND_MAX + 1.0));
      playMove (moves [index]);
      color = chooseColor (tabu);
      findMovesColor (color);
    }
  }

  bool playRandomMove (int tabu) {
    int loc [MaxSize * MaxSize], nbLocs = 0;
    for (int i = 0; i < MaxSize * MaxSize; i++) 
      if ((color [i] != 9) && (color [i] != tabu)) {
	loc [nbLocs] = i;
	nbLocs++;
      }
    Seen seen;
    Move m;
    while (nbLocs > 0) {
      int index = nbLocs * (rand () / (RAND_MAX + 1.0));
      buildMove (loc [index], seen, m);
      if (m.nbLocations > 1)
	break;
      loc [index] = loc [nbLocs - 1];
      nbLocs--;
    }
    if (m.nbLocations > 1)
      playMove (m);
    else {
      nbLocs = 0;
      for (int i = 0; i < MaxSize * MaxSize; i++) 
	if ((color [i] == tabu)) {
	  loc [nbLocs] = i;
	  nbLocs++;
	}
      while (nbLocs > 0) {
	int index = nbLocs * (rand () / (RAND_MAX + 1.0));
	buildMove (loc [index], seen, m);
	if (m.nbLocations > 1)
	  break;
	loc [index] = loc [nbLocs - 1];
	nbLocs--;
      }
      if (m.nbLocations > 1)
	playMove (m);
      else
	return false;
    }
    return true;
  }

  void playoutOptimized () {
    int secondBest = 9;
    int tabu = bestColor (secondBest);
    while (playRandomMove (tabu))
      ;
  }
};

Problem problem [MaxProblem];
HighScore highScore [MaxProblem];

void load (int nb, char *name) {
  FILE * fp = fopen (name, "r");
  if (fp != NULL) {
    for (int i = 0; i < nb; i++)
      problem [i].load (fp);
    fclose (fp);
  }
  else 
    fprintf (stderr, "pb ouverture %s\n", name);
}

void loadHighScores (int nb) {
  int sum = 0;
  for (int i = 0; i < nb; i++) {
    highScore [i].score = 0;
    highScore [i].lengthVariation = 0;
    char s [1000];
    sprintf (s, "test/high.%d", i);
    FILE * fp = fopen (s, "r");
    if (fp != NULL) {
      highScore [i].load (fp);
      fclose (fp);
    }
    sum += highScore [i].score;
  }
  fprintf (stderr, "sum = %d\n", sum);
}

void printHighScore (int i) {
  char s [1000];
  sprintf (s, "test/high.%d", i);
  FILE * fp = fopen (s, "w");
  if (fp != NULL) {
    highScore [i].print (fp);
    fclose (fp);
  }
}

int nestedRollout (Problem & pb, int n);

void chooseBestMovesSample (Problem & pb, int level, int max, Move mvs [MaxSize * MaxSize]) {
  int scoreMove [200];
  Move move [200];
  int nb = 0;

  pb.findMoves ();
  for (int i = 0; i < pb.nbMoves; i++)
    move [i] = moves [i];
  for (int i = 0; i < pb.nbMoves; i++) {
    Problem p = pb;
    p.playMove (move [i]);
    if (level >= 2) {
      scoreMove [nb] = nestedRollout (p, level - 2);
    }
    else {
      p.playout ();
      scoreMove [nb] = p.score;
    }
    nb++;
  }
  pb.nbMoves = 0;
  bool seen [200];
  for (int i = 0; i < nb; i++)
    seen [i] = false;
  for (int i = 0; i < max; i++) {
    int best = -1, m = 0;
    for (int j = 0; j < nb; j++)
      if ((scoreMove [j] > best) && !seen [j]) {
	best = scoreMove [j];
	m = j;
      }
    if (best > -1) {
      seen [m] = true;
      mvs [pb.nbMoves] = move [m];
      pb.nbMoves++;
    }
  }
}

Problem current;
int lengthBestRollout [10], scoreBestRollout [10];
Move bestRollout [10] [MaxSize * MaxSize];
Move nestedMoves [10] [MaxSize * MaxSize];

bool useBest = true;
bool useSelective = false;
bool useTabu = true;
bool useEvaluation = false;

int nestedRollout (Problem & pb, int n) {
  int bestScore, scoreRollout, bestEvaluation, secondBest;
  Move bestMove;
  int tabu = pb.bestColor (secondBest);

  if (!useTabu)
    tabu = 9;

  //pb.findMoves (tabu, secondBest);
  //pb.findMoves ();
  pb.findMoves (tabu);
  for (int i = 0; i < pb.nbMoves; i++)
    nestedMoves [n] [i] = moves [i];
  lengthBestRollout [n] = 0;
  scoreBestRollout [n] = 0;
  while (true) {
    if (pb.nbMoves == 0)
      break;
    if (useBest) {
      bestScore = scoreBestRollout [n];
      bestMove = bestRollout [n] [pb.lengthVariation];
    }
    else {
      bestScore = 0;
    }
    if (useSelective) {
      // memoriser si meilleur
/*       if (n >= 1) */
/* 	chooseBestMovesSample (pb, n , 2, nestedMoves [n]); */
/*       chooseBestMovesSample (pb, n + 1, 1, nestedMoves [n]); */
    }
    if (useEvaluation) {
      bestEvaluation = -1;
    }
    for (int i = 0; i < pb.nbMoves; i++) {
      if (n == 1) {
	Problem p = pb;
	p.playMove (nestedMoves [n] [i]);
	if (useEvaluation) {
	  int eval = p.evaluation ();
	  if (eval > bestEvaluation) {
	    bestEvaluation = eval;
	    bestMove = nestedMoves [n] [i];
	  }
	}
	else {
	  if (useTabu)
	    p.playout ();
	  else
	    p.playoutColored ();
	  //p.playoutOptimized ();
	  scoreRollout = p.score;
	  if (scoreRollout > bestScore) {
	    bestScore = scoreRollout;
	    bestMove = nestedMoves [n] [i];
	    scoreBestRollout [n] = bestScore;
	    lengthBestRollout [n] = p.lengthVariation;
	    for (int i = 0; i < p.lengthVariation; i++)
	      bestRollout [n] [i] = p.variation [i];
	  }
	}
      }
      else {
	Problem p = pb;
	p.playMove (nestedMoves [n] [i]);
	scoreRollout = nestedRollout (p, n - 1);
	if (scoreRollout > bestScore) {
	  bestScore = scoreRollout;
	  bestMove = nestedMoves [n] [i];
	  scoreBestRollout [n] = bestScore;
	  lengthBestRollout [n] = p.lengthVariation;
	  for (int i = 0; i < p.lengthVariation; i++)
	    bestRollout [n] [i] = p.variation [i];
	  if (n > 0) {
	    for (int t = 0; t < n - 1; t++)
	      fprintf (stderr, "\t");
	    fprintf (stderr, "n = %d, progress = %d, length = %d, score = %d, nbMoves = %d\n", n, pb.lengthVariation, lengthBestRollout [n], scoreBestRollout [n], pb.nbMoves);
	  }
	}
      }
    }
    pb.playMove (bestMove);
    pb.findMoves (tabu);
    //pb.findMoves ();
    for (int i = 0; i < pb.nbMoves; i++)
      nestedMoves [n] [i] = moves [i];
  }
  return pb.score;
}

int main (int argc, char ** argv) {
  load (20, "test/problems.txt");
/*   for (int i = 0; i < 10000; i++) { */
/*     Problem p = problem [0]; */
/*     p.playoutOptimized (); */
/*   } */
/*   exit (0); */

  loadHighScores (20);
  for (int i = 0; i < 20; i++)
    fprintf (stderr, "%d -> %d\n", i+1, highScore [i].score);
  for (int i = 0; i < highScore [1].lengthVariation; i++)
    highScore [1].variation [i].printCoord (stderr);
  fprintf (stderr, "\n");
/*   for (int i = 0; i < 20; i++) { */
/*     problem [i].print (stderr); */
/*     fprintf (stderr, "\n"); */
/*     problem [i].findMoves (); */
/*     fprintf (stderr, "\n\n\n"); */
/*   } */
  int nbSample = 10;
  float sum = 0;
  int maxi = 0;
/*   for (int i = 0; i < nbSample; i++) { */
/*     Problem p = problem [0]; */
/*     p.playout (); */
/*     fprintf (stderr, "%d ", p.score); */
/*     if (p.score > maxi) */
/*       maxi = p.score; */
/*     sum += p.score; */
/*   } */
/*   fprintf (stderr, "\nmean = %2.2f\nmaxi = %d\n", sum / nbSample, maxi); */
/*   sum = 0; */
/*   maxi = 0; */
/*   for (int i = 0; i < nbSample; i++) { */
/*     Problem p = problem [0]; */
/*     nestedRollout (p, 0); */
/*     fprintf (stderr, "%d ", p.score); */
/*     if (p.score > maxi) */
/*       maxi = p.score; */
/*     sum += p.score; */
/*   } */
/*   fprintf (stderr, "\nmean = %2.2f\nmaxi = %d\n", sum / nbSample, maxi); */
/*   nbSample = 1; */
/*   sum = 0; */
/*   maxi = 0; */
/*   for (int i = 0; i < nbSample; i++) { */
/*     Problem p = problem [0]; */
/*     nestedRollout (p, 1); */
/*     fprintf (stderr, "%d ", p.score); */
/*     if (p.score > maxi) */
/*       maxi = p.score; */
/*     sum += p.score; */
/*   } */
/*   fprintf (stderr, "\nmean = %2.2f\nmaxi = %d\n", sum / nbSample, maxi); */

  if (true) {
    srand (time(NULL));
    int pb = 0;
    for (int i = 0; i < 200; i++) {
      Problem p = problem [pb];
      useSelective = false;
      useTabu = true;
      nestedRollout (p, 3);
      fprintf (stderr, "\n");
      if (p.color [MaxSize * MaxSize - MaxSize] == 9)
	fprintf (stderr, "cleared!\n");
      fprintf (stderr, "score (%d) = %d\n\n", pb, p.score);
      if (p.score > highScore [pb].score) {
	highScore [pb].score = p.score;
	highScore [pb].lengthVariation = p.lengthVariation;
	for (int j = 0; j < p.lengthVariation; j++)
	  highScore [pb].variation [j] = p.variation [j];
	printHighScore (pb);
      }
    }
  }
  else {
    srand (time(NULL));
    nbSample = 1;
    useSelective = false;
    useTabu = true;
    sum = 0;
    maxi = 0;
    for (int i = 0; i < nbSample; i++) {
      for (int pb = 0; pb < 20; pb++) {
	Problem p = problem [pb];
	nestedRollout (p, 2);
	fprintf (stderr, "\n");
	if (p.color [MaxSize * MaxSize - MaxSize] == 9)
	  fprintf (stderr, "cleared!\n");
	fprintf (stderr, "score (%d) = %d\n\n", pb, p.score);
	if (p.score > highScore [pb].score) {
	  highScore [pb].score = p.score;
	  highScore [pb].lengthVariation = p.lengthVariation;
	  for (int j = 0; j < p.lengthVariation; j++)
	    highScore [pb].variation [j] = p.variation [j];
	  printHighScore (pb);
	}
      }
    }
  }
}

