exports.scrollTo = function(x) {
    return function(y) {
        return function(w) {
            return function() {
                w.scrollTo(x,y);
            }
        }
    }
}

