'use strict';

exports.createLazyLoad = function(className) {
    return function() {
        var lz = require('vanilla-lazyload');
        return new lz({
            elements_selector: className
        });
    }
}

exports.updateLazyLoad = function(instance) {
    return function() {
        instance.update();
    }
}

