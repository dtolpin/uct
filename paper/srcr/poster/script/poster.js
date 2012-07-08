
function dispall(visibility) {
    var divs = document.getElementsByTagName('div');
    for(var i=0; i!= divs.length; ++i) {
        if(divs[i].className.indexOf('region')!=-1) {
            var region = divs[i];
            region.style.display = visibility;
        }
    }
}

function zoom () {
    var im = this.className.indexOf(' maximized');
    if(im==-1) {
        dispall('none');
        this.className += ' maximized';
        this.style.display = 'block';
    } else {
        dispall('block');
        this.className = this.className.substr(0, im);
    }
}

function startup () {
    var divs = document.getElementsByTagName('div');
    for(var i=0; i!= divs.length; ++i) {
        if(divs[i].className.indexOf('zoomable')!=-1) {
            var region = divs[i];
            region.onclick = zoom;
        }
    }
}

window.onload = startup;