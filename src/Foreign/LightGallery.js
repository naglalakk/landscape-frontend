'use strict';


exports.loadGallery = function(id) {
    return function() {
        require('lightgallery.js');
        var elems = document.getElementsByClassName(id)
        for(var i = 0; i < elems.length; i++) {
            lightGallery(elems[i]);
        }
    }
}
