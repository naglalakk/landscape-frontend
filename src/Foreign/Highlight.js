'use strict';

exports.highlightBlock = function() {
    document.querySelectorAll('.ql-syntax').forEach((block) => {
        hljs.highlightBlock(block);
    });
}
