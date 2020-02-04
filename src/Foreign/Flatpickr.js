'use strict';

exports.loadFlatpickr = function(element) {
    return function() {
        var flatpickr = require('flatpickr')
        flatpickr(element, {
            enableTime: true,
            dateFormat: "d.m.Y H:i:S"
        })
    }
}
