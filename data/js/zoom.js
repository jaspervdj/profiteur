Zoom.prototype = new Model();
Zoom.prototype.constructor = Zoom;

function Zoom(zoom) {
    Model.call(this);

    this.zoom = zoom;
}

Zoom.prototype.setZoom = function(zoom) {
    if (zoom) {
        this.zoom = zoom;
        this.triggerChange();
    }
};

Zoom.prototype.getZoom = function() {
    return this.zoom;
};
