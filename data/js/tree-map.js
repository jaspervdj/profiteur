TreeMap.HORI = 0;
TreeMap.VERT = 1;

function TreeMap(resizableCanvas, selection, sorting, zoom) {
    var _this          = this;
    this.canvas        = resizableCanvas.getCanvas();
    this.rects         = {};
    this.hover         = undefined;
    this.selection     = selection;
    this.sorting       = undefined;
    this.zoom          = zoom;
    this.renderTimeout = undefined;

    $(this.canvas).mousemove(function(event) {
        var node = _this.findMouseEventNode(event);
        if (_this.hover != node) {
            _this.hover = node;  // Only update when needed
            if (node) _this.canvas.title = node.getCanonicalName();
        }
    });

    $(this.canvas).click(function(event) {
        var node = _this.findMouseEventNode(event);
        if (node) {
            // Select a node on the first click, expand it on second.
            if (node.isSelected()) {
                node.setExpanded(true);
            } else {
                node.select();
            }
        }
    });

    resizableCanvas.addChangeListener(this);
    zoom.addChangeListener(this);
    sorting.addChangeListener(this);
    this.onChange(zoom);
}

TreeMap.prototype.mkRect = function(x, y, w, h) {
    if (x == null || y == null || w == null || h == null) {
        throw 'foo';
    }

    return {
        x: x,
        y: y,
        w: w,
        h: h
    };
};

TreeMap.prototype.mkRootRect = function() {
    return this.mkRect(0, 0, this.canvas.width, this.canvas.height);
};

TreeMap.prototype.findMouseEventNode = function(event) {
    var _this  = this;
    var offset = $(this.canvas).offset();
    var x      = event.pageX - offset.left;
    var y      = event.pageY - offset.top;

    function find(node) {
        for (var i = 0; i < node.children.length; i++) {
            var r = _this.rects[node.children[i].id];
            if (r && x >= r.x && x < r.x + r.w &&
                    y >= r.y && y < r.y + r.h) {
                return find(node.children[i]);
            }
        }

        return node;
    }

    return node = this.zoom.getZoom() ? find(this.zoom.getZoom()) : undefined;
};

TreeMap.prototype.layoutChildren = function(node) {
    var _this = this;

    if (node.children.length > 0 && _this.rects[node.children[0].id]) {
        return;  // Already layed out.
    }

    // Sum of areas
    function sumCosts(nodes) {
        var sum = 0;
        for (var i = 0; i < nodes.length; i++) sum += nodes[i].getCost();
        return sum;
    }

    // Assume that nodes is not empty. Calculates the worst aspect ratio
    // when laying out the given nodes along the given side.
    function worst(nodes, side) {
        var cost = sumCosts(nodes);

        function aspect(node) {
            return Math.max(
                    node.getCost() / cost,
                    cost / node.getCost());
        }

        var max = aspect(nodes[0]);
        for (var i = 1; i < nodes.length; i++) {
            var a = aspect(nodes[i]);
            if (a > max) max = a;
        }

        return max;
    }

    // Layout nodes after a split in the given direction.
    function layoutRow(rect, nodes, dir) {
        var cost = sumCosts(nodes);
        var x    = rect.x;
        var y    = rect.y;
        for (var i = 0; i < nodes.length; i++) {
            if (dir === TreeMap.HORI) {
                var w = rect.w;
                var h = rect.h * nodes[i].getCost() / cost;
                _this.rects[nodes[i].id] = _this.mkRect(x, y, w, h);
                y += h;
            } else {
                var w = rect.w * nodes[i].getCost() / cost;
                var h = rect.h;
                _this.rects[nodes[i].id] = _this.mkRect(x, y, w, h);
                x += w;
            }
        }
    }

    // Main recursive worker. Nodes should not be empty.
    function subdivide(rect, nodes, row, side, dir) {
        var n = nodes[0];

        if (row.length <= 0 ||
                worst(row, side) >= worst(row.concat([n]), side)) {
            var remaining = nodes.slice(1);
            if (remaining.length <= 0) {
                layoutRow(rect, row.concat([n]), dir);
            } else {
                subdivide(rect, remaining, row.concat([n]), side, dir);
            }
        } else {
            var fillCost = sumCosts(row);
            var nextCost = sumCosts(nodes);
            var cost     = fillCost + nextCost;
            var fillRect = undefined;
            var nextRect = undefined;
            if (dir === TreeMap.HORI) {
                var w = rect.w * fillCost / cost;
                fillRect = _this.mkRect(rect.x, rect.y, w, rect.h);
                nextRect = _this.mkRect(
                        rect.x + w, rect.y, rect.w - w, rect.h);
            } else {
                var h = rect.h * fillCost / cost;
                fillRect = _this.mkRect(rect.x, rect.y, rect.w, h);
                nextRect = _this.mkRect(
                        rect.x, rect.y + h, rect.w, rect.h - h);
            }

            layoutRow(fillRect, row, dir);
            squarify(nextRect, nodes);
        }
    }

    // Top-level layouter
    function squarify(rect, nodes) {
        var cost = sumCosts(nodes);
        var dir  = undefined;
        var side = undefined;
        if (rect.w > rect.h) {
            dir  = TreeMap.HORI;
            side = Math.sqrt(cost * rect.w / rect.h);
        } else {
            dir  = TreeMap.VERT;
            side = Math.sqrt(cost * rect.h / rect.w);
        }

        subdivide(rect, nodes, [], side, dir);
    }

    // Go!
    squarify(_this.rects[node.id], node.children);
};

TreeMap.prototype.render = function() {
    var _this   = this;
    window.clearTimeout(_this.renderTimeout);
    _this.renderTimeout = window.setTimeout(function() {
        var context = _this.canvas.getContext('2d');

        // Clear frame
        context.fillStyle = '#ffffff';
        context.fillRect(0, 0, _this.canvas.width, _this.canvas.height);

        // Recursively render all nodes
        _this.renderNode(_this.zoom.getZoom());

        // Render selection
        var selected = _this.selection.getSelectedNode()
        if (selected && _this.rects[selected.id]) {
            var rect = _this.rects[selected.id];
            context.lineWidth   = 3;
            context.strokeStyle = '#ffffff';
            context.strokeRect(rect.x, rect.y, rect.w, rect.h);
        }
    }, 50);
};

TreeMap.prototype.renderNode = function(node) {
    var _this   = this;
    var context = this.canvas.getContext('2d');
    var rect    = this.rects[node.id];

    // The user is not going to see it anyway.
    if (!rect || rect.w < 2 || rect.h < 2) return;

    // Fill entire area
    context.fillStyle = node.getColor();
    context.fillRect(rect.x, rect.y, rect.w, rect.h);
    context.strokeStyle = 'black';
    context.strokeRect(rect.x, rect.y, rect.w, rect.h);

    // Draw children on top.
    if (node.children.length > 0) {
        _this.layoutChildren(node);
        for (var i = 0; i < node.children.length; i++) {
            _this.renderNode(node.children[i]);
        }
    }
};

TreeMap.prototype.onChange = function(source) {
    var _this = this;

    if (source instanceof ResizingCanvas ||
            source instanceof Sorting ||
            source instanceof Zoom) {
        var node = _this.zoom.getZoom();
        _this.rects = {};
        _this.rects[node.id] = _this.mkRootRect();
        _this.render();
        node.addChangeListener(this);
    } else if (source instanceof Node) {
        _this.render();
        for (var i = 0; i < source.children.length; i++) {
            source.children[i].addChangeListener(this);
        }
    }
};
