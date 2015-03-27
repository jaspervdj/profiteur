IndividualNode.prototype = new Model();
IndividualNode.prototype.constructor = IndividualNode;

function IndividualNode(prof, selection, sorting, parent, id) {
    Model.call(this);

    this.prof      = prof;
    this.sorting   = sorting;
    this.parent    = parent;
    this.id        = id + '.individual';
    this.data      = prof.nodes[id];
    this.expanded  = false;
    this.children  = [];
    this.selection = selection;

    sorting.addChangeListener(this);
}

IndividualNode.prototype.isExpandable = function() {
    return false;
};

IndividualNode.prototype.toggleExpanded = function() {
    return;
};

IndividualNode.prototype.computeChildren = function() {
    return;
};

IndividualNode.prototype.setExpanded = function(expanded) {
    return;
};

IndividualNode.prototype.getCanonicalName = function() {
    return this.data.name.canonical + ' (indiv)';
};

IndividualNode.prototype.getFullName = function() {
    return this.data.name.module + '.' + this.getCanonicalName();
};

IndividualNode.prototype.getColor = function() {
    return '#222222';
};

IndividualNode.prototype.getCost = function() {
    return this.sorting.getCost(this);
};

IndividualNode.prototype.getTime = function() {
    return this.data.info.individualTime;
};

IndividualNode.prototype.getAlloc = function() {
    return this.data.info.individualAlloc;
};

IndividualNode.prototype.isSelected = function() {
    return this == this.selection.getSelectedNode();
};

IndividualNode.prototype.select = function() {
    this.selection.setSelectedNode(this);
};

IndividualNode.prototype.onChange = function(sorting) {
    this.triggerChange();
};
