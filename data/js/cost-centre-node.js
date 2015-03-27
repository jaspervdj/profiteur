CostCentreNode.prototype = new Model();
CostCentreNode.prototype.constructor = CostCentreNode;

function CostCentreNode(prof, selection, sorting, parent, id) {
    Model.call(this);

    this.prof      = prof;
    this.sorting   = sorting;
    this.parent    = parent;
    this.id        = id;
    this.data      = prof.nodes[id];
    this.expanded  = false;
    this.children  = [];
    this.selection = selection;

    sorting.addChangeListener(this);
}

CostCentreNode.prototype.isExpandable = function() {
    return true;
};

CostCentreNode.prototype.toggleExpanded = function() {
    this.setExpanded(!this.expanded);
};

CostCentreNode.prototype.computeChildren = function() {
    var _this = this;

    if (_this.expanded && _this.children.length <= 0) {
        _this.children.push(new IndividualNode(
                _this.prof, _this.selection, _this.sorting,
                _this.parent, _this.id));

        for (var i = 0; i < _this.data.children.length; i++) {
            _this.children.push(new CostCentreNode(
                    _this.prof, _this.selection, _this.sorting,
                    _this, _this.data.children[i]));
        }
    } else if (!_this.expanded && _this.children.length > 0) {
        _this.children = [];
    }

    _this.children.sort(function(a, b) {
        return b.getCost() - a.getCost();
    });
};

CostCentreNode.prototype.setExpanded = function(expanded) {
    var _this = this;

    if (_this.expanded === expanded) return;

    _this.expanded = expanded;
    _this.computeChildren();

    _this.triggerChange();
};

CostCentreNode.prototype.getCanonicalName = function() {
    return this.data.name.canonical;
};

CostCentreNode.prototype.getFullName = function() {
    return this.data.name.module + '.' + this.getCanonicalName();
};

CostCentreNode.prototype.getColor = function() {
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

CostCentreNode.prototype.getCost = function() {
    return this.sorting.getCost(this);
};

CostCentreNode.prototype.getTime = function() {
    return this.data.info.inheritedTime;
};

CostCentreNode.prototype.getAlloc = function() {
    return this.data.info.inheritedAlloc;
};

CostCentreNode.prototype.isSelected = function() {
    return this == this.selection.getSelectedNode();
};

CostCentreNode.prototype.select = function() {
    this.selection.setSelectedNode(this);
};

CostCentreNode.prototype.onChange = function(sorting) {
    this.computeChildren();
    this.triggerChange();
};
