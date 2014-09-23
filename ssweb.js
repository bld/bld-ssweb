function planet(rad, col) {
    var pMaterial = new THREE.LineBasicMaterial({ color : col });
    var pGeometry = new THREE.CircleGeometry(rad, 100);
    pGeometry.vertices.shift();
    return new THREE.Line(pGeometry, pMaterial);
};
function makePlanet(rad, col) {
    var pMaterial = new THREE.LineBasicMaterial({ color : col });
    var pGeometry = new THREE.CircleGeometry(rad, 100);
    pGeometry.vertices.shift();
    return new THREE.Line(pGeometry, pMaterial);
};
function plot(document) {
    var p = { width : 640,
              height : 640,
              fov : 75,
              near : 1,
              far : 4000,
              origin : new THREE.Vector3(0, 0, 0),
              renderer : new THREE.WebGLRenderer(),
              trajDiv : document.getElementById('trajectory'),
              camera : null,
              scene : new THREE.Scene(),
              traj3d : null,
              trajseg : null,
              geometry : null,
              material : null,
              node : null,
              nodes : null,
              planets : { sun : new planet(0.01, 0xffff00),
                          mercury : new planet(0.387, 0x888888),
                          venus : new planet(0.723, 0xffff44),
                          earth : new planet(1, 0x0000ff),
                          mars : new planet(1.5, 0xff0000),
                          ceres : new planet(2.77, 0x666666),
                          jupiter : new planet(5.2, 0xff9900)
                        }
            };
    p.renderer.setSize(p.width, p.height);
    p.trajDiv.appendChild(p.renderer.domElement);
    p.camera = new THREE.PerspectiveCamera(p.fov, p.width / p.height, p.near, p.far);
    p.camera.position.set(0, 0, 10);
    p.camera.lookAt(p.origin);
    p.scene.add(p.camera);
    for (var pl in p.planets) {
        p.scene.add(p.planets[pl]);
    };
    return p;
};
var ssplot = new plot(document);
function updateTrajectory(plot, document) {
    var lightness = document.getElementsByName('lightness')[0].valueAsNumber;
    var angles1890 = (function () {
        var angles = document.getElementsByName('angles[]');
        var _js1885 = angles.length;
        var collect1886 = [];
        for (var _js1884 = 0; _js1884 < _js1885; _js1884 += 1) {
            var angle = angles[_js1884];
            collect1886['push'](angle.valueAsNumber * 0.017453292519943295);
        };
        return collect1886;
    })();
    var durations1891 = (function () {
        var durations = document.getElementsByName('durations[]');
        var _js1888 = durations.length;
        var collect1889 = [];
        for (var _js1887 = 0; _js1887 < _js1888; _js1887 += 1) {
            var duration = durations[_js1887];
            collect1889['push'](duration.valueAsNumber * 6.283185307179586);
        };
        return collect1889;
    })();
    return $.getJSON('ssprop', { lightness : lightness,
                                 angles : angles1890,
                                 durations : durations1891
                               }, function (trajectory) {
        if (plot.traj3d) {
            plot.scene.remove(plot.traj3d);
        };
        if (plot.nodes) {
            for (var i = 0; i < plot.nodes.length; i += 1) {
                plot.node = plot.nodes[i];
                plot.scene.remove(plot.node);
            };
        };
        plot.nodes = [];
        plot.geometry = new THREE.Geometry();
        plot.material = new THREE.LineBasicMaterial({ color : 0x00ff00 });
        plot.geometry.vertices = (function () {
            var rmax = 0;
            var _js1893 = trajectory.length;
            var append1894 = [];
            for (var _js1892 = 0; _js1892 < _js1893; _js1892 += 1) {
                with ({ segment : null }) {
                    var segment = trajectory[_js1892];
                    var tmpg = new THREE.BoxGeometry(1, 1, 1);
                    var tmpm = new THREE.MeshBasicMaterial({ color : 0x00ff00 });
                    append1894 = append1894.concat((function () {
                        var _js1896 = segment.length;
                        var collect1897 = [];
                        for (var _js1895 = 0; _js1895 < _js1896; _js1895 += 1) {
                            var _db1898 = segment[_js1895];
                            var tm = _db1898[0];
                            var x = _db1898[1];
                            var y = _db1898[2];
                            var v = new THREE.Vector3(x, y, 0);
                            collect1897['push'](v);
                            rmax = Math.max(rmax, v.length());
                        };
                        var rcam = 1.1 * (rmax / Math.tan((plot.fov * (Math.PI / 180)) / 2));
                        plot.camera.position.set(0, 0, rcam);
                        return collect1897;
                    })());
                    plot.node = new THREE.Mesh(tmpg, tmpm);
                    plot.node.position.x = segment[0][1];
                    plot.node.position.y = segment[0][2];
                    plot.node.scale.x = rmax / 50;
                    plot.node.scale.y = rmax / 50;
                    plot.node.scale.z = rmax / 50;
                    plot.scene.add(plot.node);
                    plot.nodes.push(plot.node);
                };
            };
            return append1894;
        })();
        plot.traj3d = new THREE.Line(plot.geometry, plot.material);
        plot.scene.add(plot.traj3d);
        return plot.renderer.render(plot.scene, plot.camera);
    });
};
var controlCount = 1;
function newControl(document, plot) {
    ++controlCount;
    var div1 = document.createElement('div');
    div1.id = controlCount;
    div1.innerHTML = '<TABLE><TR><TD>Angle</TD><TD><INPUT TYPE="number" MIN="-90" MAX="90" STEP="5" VALUE="0" NAME="angles[]"></TD><TD>Duration</TD><TD><INPUT TYPE="number" MIN="0" MAX="10" STEP="0.1" VALUE="1" NAME="durations[]"></TD><TD><DIV CLASS="delete"><B>Delete</B></DIV></TD></TR></TABLE>';
    document.getElementById('controls').appendChild(div1);
    return updateTrajectory(plot, document);
};
function deleteControl(elementId, document, plot) {
    var d = document;
    var element = d.getElementById(elementId);
    var parentElement = d.getElementById('controls');
    parentElement.removeChild(element);
    return updateTrajectory(plot, document);
};
$(document).ready(updateTrajectory(ssplot, document));
function updateInputEvents() {
    return $('input').change(function () {
        return updateTrajectory(ssplot, document);
    });
};
function updateDeleteEvents() {
    return $('.delete').click(function () {
        var elementId = this.parentElement.parentElement.parentElement.parentElement.parentElement.id;
        return deleteControl(elementId, document, ssplot);
    });
};
updateInputEvents();
$('#update').click(function () {
    return updateTrajectory(ssplot, document);
});
$('#addcontrol').click(function () {
    newControl(document, ssplot);
    updateInputEvents();
    return updateDeleteEvents();
});
