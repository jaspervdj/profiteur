Node.prototype = new Model();
Node.prototype.constructor = Node;

function Node(prof, selection, sorting, parent, id) {
    Model.call(this);

    this.prof      = prof;
    this.sorting   = sorting;
    this.parent    = parent;
    this.id        = id;
    this.data      = prof ? prof.nodes[id] : undefined;
    this.expanded  = false;
    this.children  = [];
    this.selection = selection;

    if (sorting) sorting.addChangeListener(this);
}

Node.prototype.isExpandable = function() {
    return false;
};

Node.prototype.toggleExpanded = function() {
    this.setExpanded(!this.expanded);
};

Node.prototype.computeChildren = function() {
    return;
};

Node.prototype.setExpanded = function(expanded) {
    var _this = this;

    if (_this.expanded === expanded) return;

    _this.expanded = expanded;
    _this.computeChildren();

    _this.triggerChange();
};

Node.prototype.getCanonicalName = function() {
    return this.data.name.canonical;
};

Node.prototype.getFullName = function() {
    return this.data.name.module + '.' + this.getCanonicalName();
};

Node.prototype.getColor = function() {
    var hash = 5381;
    var str  = this.data.name.module + '.' + this.data.name.canonical;
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
    return 0;
};

Node.prototype.getAlloc = function() {
    return 0;
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
