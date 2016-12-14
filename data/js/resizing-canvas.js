ResizingCanvas.prototype = new Model();
ResizingCanvas.prototype.constructor = ResizingCanvas;

function ResizingCanvas(container) {
    Model.call(this);

    this.container = container;
    this.canvas    = document.createElement('canvas');
    container.append($(this.canvas));

    var _this = this;
    window.setInterval(function() {
        _this.checkSize();
    }, 300);
}

ResizingCanvas.prototype.checkSize = function() {
    var width  = Math.floor(this.container.width());
    var height = Math.floor(this.container.height());
    if (width !== this.canvas.width || height !== this.canvas.height) {
        this.canvas.width  = width;
        this.canvas.height = height;
        this.triggerChange();
    }
};

ResizingCanvas.prototype.getCanvas = function() {
    return this.canvas;
};
