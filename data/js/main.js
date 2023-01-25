////////////////////////////////////////////////////////////////////////////////
// Hooking up
////////////////////////////////////////////////////////////////////////////////

function main() {
    var selection = new Selection();
    var sorting = new Sorting();
    var root = new Node($prof[0], selection, sorting, undefined, $prof[1]);

    var zoom = new Zoom(root);
    var tb = new TreeBrowser($('#tree'), zoom);
    var rc = new ResizingCanvas($('#map'));
    var tm = new TreeMap(rc, selection, sorting, zoom);
    var details = new Details($('#details'), selection, sorting, zoom);
    selection.setSelectedNode(root);

    let currentNode = root;
    currentNode.select();
    currentNode.setExpanded(true);
    while(currentNode.children.length == 1) {
        currentNode = currentNode.children[0];
        currentNode.select();
        currentNode.setExpanded(true);
    }
    tb.scrollToNode(currentNode, true);
}

$(document).ready(function () {
    // alert('Hello wooorld');
    main();
});
