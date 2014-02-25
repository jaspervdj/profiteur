Selection.prototype = new Model();
Selection.prototype.constructor = Selection;

function Selection() {
    Model.call(this);

    this.selectedNode = undefined;
}

Selection.prototype.getSelectedNode = function() {
    return this.selectedNode;
};

Selection.prototype.setSelectedNode = function(node) {
    if (this.selectedNode == node) return;

    var previousSelectedNode = this.selectedNode;
    this.selectedNode = node;
    if (previousSelectedNode) previousSelectedNode.triggerChange();
    if (node) node.triggerChange();
    this.triggerChange();
};
