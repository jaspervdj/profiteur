Sorting.prototype = new Model();
Sorting.prototype.constructor = Sorting;

Sorting.methods = {
    'by-time': {
        name:    'View by time',
        getCost: function(node) {
            return node.getTime();
        }
    },
    'by-alloc': {
        name:    'View by alloc',
        getCost: function(node) {
            return node.getAlloc();
        }
    }
};

function Sorting() {
    Model.call(this);

    this.method = Sorting.methods['by-time'];
}

Sorting.prototype.setMethodByKey = function(key) {
    this.setMethod(Sorting.methods[key]);
};

Sorting.prototype.setMethod = function(method) {
    this.method = method;
    this.triggerChange();
};

Sorting.prototype.getCost = function(node) {
    return this.method.getCost(node);
};
