'use strict';

exports.setHTML = function(el) {
    return function (html) {
        return function() {
            el.innerHTML = html;
        };
    };
};
