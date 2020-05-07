'use strict';

var lazyLoadInstance;

exports.lazyLoad = function(className) {
    return function() {
        var lz = require('vanilla-lazyload');
        lazyLoadInstance = new lz({
            elements_selector: className
        });
    }
}

exports.lazyLoadUpdate = function() {
    lazyLoadInstance.update();
}

