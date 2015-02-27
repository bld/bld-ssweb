var time;

function toggleDiv (divId) {
    $('#' + divId).toggle();
};

function Sail () {
    var g = new THREE.Geometry(),
	w = 8,
	w2 = w/2,
	m = new THREE.MeshBasicMaterial({color : 0x444444, side : THREE.DoubleSide});
    g.vertices.push(
	new THREE.Vector3(0, 0, 0),
	new THREE.Vector3(w2, 0, 0),
	new THREE.Vector3(0, 0, w2),
	new THREE.Vector3(-w2, 0, 0),
	new THREE.Vector3(0, 0, -w2));
    g.faces.push(
	new THREE.Face3(0, 1, 2),
	new THREE.Face3(0, 2, 3),
	new THREE.Face3(0, 3, 4),
	new THREE.Face3(0, 4, 1));
    g.computeFaceNormals();
    g.computeBoundingSphere();
    return new THREE.Mesh(g, m);
};

function onWindowResize () {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
    camera.lookAt(origin);
    //if (controls) {
    //controls.handleResize();
    //}
    render();
}

function init() {
    scene = new THREE.Scene();
    origin = new THREE.Vector3(0,0,0);
    plotDiv = document.getElementById('plot');
    if (window.WebGLRenderingContext) {
	renderer = new THREE.WebGLRenderer({ antialias : true })
    } else {
	renderer = new THREE.CanvasRenderer();
    };
    renderer.setSize(window.innerWidth, window.innerHeight);
    plotDiv.appendChild(renderer.domElement);
    camera = new THREE.PerspectiveCamera(75, window.innerWidth/window.innerHeight, 0.1, 1000);
    camera.position.set(2, -6, 3);
    camera.lookAt(origin);
    scene.add(camera);
    sail = new Sail();
    scene.add(sail);
    window.addEventListener('resize', onWindowResize, false);
    toggleDiv('helpText');
    vel = 0;
    pos = 0;
};

function render () {
    renderer.render(scene, camera);
};

function update () {
    now = (new Date()).getTime();
    dt = now - (time || now);
    accel = 1e-7;
    time = now;
    vel += accel * dt;
    pos += vel * dt;
    sail.position.y = pos;
};

function initTiltControls () {
    incidence = 0;
    rotation = 0;
    absorbed = 1;
    $(document.body).on(
	'keydown',
	function (e) {
	    switch (e.which) {
	    case 37: return left();
	    case 39: return right();
	    case 38: return up();
	    case 40: return down();
	    };
	});
    $('#up').click(function (e) { return up(); });
    $('#down').click(function (e) { return down(); });
    $('#left').click(function (e) { return left(); });
    $('#right').click(function (e) { return right(); });
};

function calcAbsorbed(incidence) {
    return Math.abs(Math.round(100 * Math.cos(incidence * (Math.PI / 180))));
};

function rotateGlobalY(object, rad) {
    var gy = new THREE.Vector3(0, 1, 0);
    var ly = object.worldToLocal(gy);
    ly.normalize();
    return object.rotateOnAxis(ly, rad);
};

function up() {
    sail.rotateZ(Math.PI / 36);
    incidence += 5;
    if (incidence > 180) {
        incidence -= 360;
    };
    absorbed = calcAbsorbed(incidence);
    sail.updateMatrixWorld();
};

function down() {
    sail.rotateZ(Math.PI / -36);
    incidence -= 5;
    if (incidence < -180) {
        incidence = 360 + incidence;
    };
    absorbed = calcAbsorbed(incidence);
    sail.updateMatrixWorld();
};

function left() {
    rotateGlobalY(sail, Math.PI / 36);
    rotation -= 5;
    if (rotation < -180) {
        rotation = 360 + rotation;
    };
    sail.updateMatrixWorld();
};

function right() {
    rotateGlobalY(sail, Math.PI / -36);
    rotation += 5;
    if (rotation > 180) {
        rotation -= 360;
    };
    sail.updateMatrixWorld();
};

function animate () {
    requestAnimationFrame(animate);
    update();
    render();
};
