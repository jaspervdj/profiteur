function mk(el) {
    return $(document.createElement(el));
}

function TreeBrowser(container, zoom) {
    this.zoom      = zoom;
    this.elements  = {};
    this.container = container;

    zoom.addChangeListener(this);
    this.onChange(zoom);
}

TreeBrowser.prototype.mkElement = function(node) {
    var div = mk('div').addClass('tree-browser node');

    if (node.isExpandable()) {
        var chevron = mk('span').addClass('tree-browser chevron');
        chevron.click(function() { node.toggleExpanded(); });
        div.append(chevron);
    }

    var code = mk('code').text(node.getCanonicalName());
    code.addClass('tree-browser name');
    code.attr('style', 'background-color: ' + node.getColor());
    code.click(function() { node.select(); });
    div.append(code);

    var children = mk('div').addClass('tree-browser children');
    div.append(children);

    return div;
};

TreeBrowser.prototype.renderNode = function(element, node) {
    if (node.isExpandable()) {
        element.children('.chevron').text(
                node.expanded ? Unicode.DOWN_TRIANGLE : Unicode.RIGHT_TRIANGLE);
    }

    element.children('.name').toggleClass('selected', node.isSelected());

    var children = element.children('.children');
    if (node.children.length > 0) {
        if (children.is(':empty')) {
            for (var i = 0; i < node.children.length; i++) {
                var div = this.mkElement(node.children[i]);
                this.elements[node.children[i].id] = div;
                this.renderNode(div, node.children[i]);
                children.append(div);
            }
        } else {
            children.children().detach();
            for (var i = 0; i < node.children.length; i++) {
                var div = this.elements[node.children[i].id];
                children.append(div);
            }
        }
    }

    if (node.children.length <= 0 && !children.is(':empty')) {
        children.empty();
    }
};

TreeBrowser.prototype.scrollToNode = function(node) {
    var element  = this.elements[node.id];
    var position = element.position();
    var top      = this.container.scrollTop();
    var left     = this.container.scrollLeft();
    var height   = this.container.height();
    var width    = this.container.width();

    if (position.top < 0 || position.top >= height ||
            position.left < 0 || position.left >= width) {
        this.container.scrollTop(top + position.top - height / 2);
        this.container.scrollLeft(left + position.left);
    }
};

TreeBrowser.prototype.onChange = function(source) {
    var _this   = this;
    if (source instanceof Zoom) {
        var node = _this.zoom.getZoom();
        _this.container.empty();
        _this.elements = [];
        _this.elements[node.id] = _this.mkElement(node);
        _this.container.append(_this.elements[node.id]);
        _this.renderNode(_this.elements[node.id], node);
        node.addChangeListener(_this);
    } else if (source instanceof Node) {
        var element = _this.elements[source.id];

        if (!element) return;  // Invisible source changed what?

        _this.renderNode(element, source);
        for (var i = 0; i < source.children.length; i++) {
            source.children[i].addChangeListener(_this);
        }

        if (source.isSelected()) _this.scrollToNode(source);
    }
}
