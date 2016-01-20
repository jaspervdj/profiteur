Node.prototype = new Model();
Node.prototype.constructor = Node;

function Node(prof, selection, sorting, parent, id) {
    Model.call(this);
    var _this = this;

    // Set general properties
    _this.prof      = prof;
    _this.sorting   = sorting;
    _this.parent    = parent;
    _this.id        = id;
    _this.expanded  = false;
    _this.selection = selection;

    // Set data
    var data = prof[id];
    _this.name     = data[0];
    _this.module   = data[1];
    _this.entries  = data[2];
    _this.time     = data[3];
    _this.alloc    = data[4];
    _this.childIds = data[5];
    _this.children = [];

    if (sorting) sorting.addChangeListener(_this);
}

Node.prototype.isExpandable = function() {
    return this.childIds.length > 0;
};

Node.prototype.toggleExpanded = function() {
    this.setExpanded(!this.expanded);
};

Node.prototype.computeChildren = function() {
    var _this = this;

    if (_this.expanded && _this.children.length <= 0) {
        _this.children = [];
        for (var i in _this.childIds) {
            var childId = _this.childIds[i];
            _this.children.push(new Node(
                    _this.prof, _this.selection, _this.sorting,
                    _this, childId));
        }
    } else if (!_this.expanded && _this.children.length > 0) {
        _this.children = [];
    }

    _this.children.sort(function(a, b) {
        return b.getCost() - a.getCost();
    });
};

Node.prototype.setExpanded = function(expanded) {
    var _this = this;
    if (_this.expanded === expanded) return;
    _this.expanded = expanded;
    _this.computeChildren();
    _this.triggerChange();
};

Node.prototype.getCanonicalName = function() {
    return this.name;
};

Node.prototype.getModuleName = function() {
    return this.module;
};

Node.prototype.getFullName = function() {
    return this.module + '.' + this.name;
};

Node.prototype.getEntries = function() {
    return this.entries;
};

Node.prototype.getColor = function() {
    var hash = 5381;
    var str  = this.getFullName();
    for (var i = 0; i < str.length; i++) {
        hash = (hash << 5) + hash + str.charCodeAt(i);
    }

    hash = hash % 0xffffff;
    var r = (hash & 0xff0000) >> 16;
    var g = (hash & 0x00ff00) >> 8;
    var b = (hash & 0x0000ff);

    r = (r) / 2;
    g = (g) / 2;
    b = (b) / 2;

    function hex2(x) {
        var str = x.toString(16);
        if (str.length < 2) {
            for (var i = 0; i < 2 - str.length; i++) {
                str = '0' + str;
            }
        }
        return str;
    }

    return '#' +
        hex2(Math.round(r)) +
        hex2(Math.round(g)) +
        hex2(Math.round(b));
};

Node.prototype.getCost = function() {
    return this.sorting.getCost(this);
};

Node.prototype.getTime = function() {
    return this.time;
};

Node.prototype.getAlloc = function() {
    return this.alloc;
};

Node.prototype.isSelected = function() {
    return this == this.selection.getSelectedNode();
};

Node.prototype.select = function() {
    this.selection.setSelectedNode(this);
};

Node.prototype.onChange = function(sorting) {
    this.computeChildren();
    this.triggerChange();
};
