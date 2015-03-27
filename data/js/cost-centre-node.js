CostCentreNode.prototype = new Node();
CostCentreNode.prototype.constructor = CostCentreNode;

function CostCentreNode(prof, selection, sorting, parent, id) {
    Node.call(this, prof, selection, sorting, parent, id);
}

CostCentreNode.prototype.isExpandable = function() {
    return true;
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

CostCentreNode.prototype.getTime = function() {
    return this.data.info.inheritedTime;
};

CostCentreNode.prototype.getAlloc = function() {
    return this.data.info.inheritedAlloc;
};
