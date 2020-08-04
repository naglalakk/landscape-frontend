'use strict';

exports.createTagify = function(element) {
    return function() {
        var tagify = require('@yaireo/tagify');
        var elem = document.getElementById(element);
        return new tagify(elem);
    }
}

exports.addTags = function(instance) {
    return function(tags) {
        return function() {
            instance.addTags(tags);
        }
    }
}

exports.getTags = function(instance) {
    return function() {
        return instance.value.map(x => x.value)
    }
}
