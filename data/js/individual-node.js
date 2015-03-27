IndividualNode.prototype = new Node();
IndividualNode.prototype.constructor = IndividualNode;

function IndividualNode(prof, selection, sorting, parent, id) {
    Node.call(this, prof, selection, sorting, parent, id);

    this.id = id + '.individual';
}

IndividualNode.prototype.isExpandable = function() {
    return false;
};

IndividualNode.prototype.getCanonicalName = function() {
    return '(indiv)';
};

IndividualNode.prototype.getTime = function() {
    return this.data.info.individualTime;
};

IndividualNode.prototype.getAlloc = function() {
    return this.data.info.individualAlloc;
};
