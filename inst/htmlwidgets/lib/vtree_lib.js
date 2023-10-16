function branch_segment(d1, d2, length, rad_seg, material, is_capped = true) {
  const seg_geom = new THREE.CylinderGeometry(d2, d1, length, rad_seg, 1, true);
  const seg_mesh = new THREE.Mesh(seg_geom, material);
  seg_mesh.translateY(length / 2);
  seg_mesh.castShadow = true;
  seg_mesh.receiveShadow = true;
  const brg = new THREE.Group();
  brg.add(seg_mesh);
  if (is_capped) {
    const cap_geom = new THREE.ConeGeometry(d2, d2, rad_seg);
    const cap_mesh = new THREE.Mesh(cap_geom, material);
    cap_mesh.translateY(length + d2 / 2);
    brg.add(cap_mesh);
  }
  return brg;
}

const BARK_TYPES = ["bark_1", "bark_2", "bark_3"];
const LEAVES_TYPES = ["leaves_1"];
const FOLDER_LIB = "lib/threejs-156/";

const LIB_PATH =
  "https://raw.githubusercontent.com/degi/forcastree/main/images/";
const def_leaves_path = LIB_PATH.concat("leaves_1.png");
const def_leaves_bump_path = LIB_PATH.concat("leaves_1_bump.png");
const def_bark_path = LIB_PATH.concat("bark.png");
const def_bark_bump_path = LIB_PATH.concat("bark_bump.png");
const def_ground_path = LIB_PATH.concat("ground.png");
const def_ground_bump_path = LIB_PATH.concat("ground_bump.png");
const def_plants = [
  LIB_PATH.concat("plant_1.png"),
  LIB_PATH.concat("plant_2.png"),
  LIB_PATH.concat("plant_3.png"),
  LIB_PATH.concat("plant_4.png"),
  LIB_PATH.concat("plant_5.png"),
  LIB_PATH.concat("plant_6.png"),
  LIB_PATH.concat("plant_7.png"),
];

function get_branches_material(
  bark_type = "bark",
  height_repeat = 3,
  tex_path = def_bark_path,
  bump_path = def_bark_bump_path
) {
  const trunk_tex = new THREE.TextureLoader().load(tex_path);
  const trunk_bump = new THREE.TextureLoader().load(bump_path);
  trunk_tex.wrapS = THREE.RepeatWrapping;
  trunk_tex.wrapT = THREE.RepeatWrapping;
  trunk_tex.repeat.set(2, height_repeat);
  trunk_bump.wrapS = THREE.RepeatWrapping;
  trunk_bump.wrapT = THREE.RepeatWrapping;
  trunk_bump.repeat.set(2, height_repeat);
  const trunk_mat = new THREE.MeshLambertMaterial({ map: trunk_tex });
  trunk_mat.bumpMap = trunk_bump;
  trunk_mat.bumpScale = 0.2;
  return trunk_mat;
}

function get_leaves_material(
  leaves_type = "leaves_1",
  tex_path = def_leaves_path,
  bump_path = def_leaves_bump_path
) {
  if (!LEAVES_TYPES.includes(leaves_type)) leaves_type = "leaves_1";
  const tex_file = FOLDER_LIB.concat(leaves_type, ".png");
  const bum_file = FOLDER_LIB.concat(leaves_type, "_bump.png");
  const leaves_tex = new THREE.TextureLoader().load(tex_path);
  const leaves_bump = new THREE.TextureLoader().load(bump_path);
  const leaves_mat = new THREE.MeshLambertMaterial({ map: leaves_tex });
  leaves_mat.side = THREE.DoubleSide;
  leaves_mat.transparent = true;
  leaves_mat.alphaTest = 0.5;
  leaves_mat.bumpMap = leaves_bump;
  leaves_mat.bumpScale = 0.1;
  return leaves_mat;
}

function rad_seg(d) {
  return Math.min(9, Math.max(3, Math.round(d * 50)));
}

function generate_branches_obj(
  branches,
  bark_type,
  bark_mat_long,
  bark_mat_short,
  is_branch_capped = true
) {
  // const mat_long = get_branches_material(bark_type, 3)
  // const mat_short = get_branches_material(bark_type, 1)
  const mat_long = bark_mat_long;
  const mat_short = bark_mat_short;

  var branches_mat = mat_long;
  const quaternion_z = new THREE.Quaternion();
  const quaternion_y = new THREE.Quaternion();
  const axis_z = new THREE.Vector3(0, 0, 1);
  const axis_y = new THREE.Vector3(0, 1, 0);
  const br_group = new THREE.Group();
  for (var i = 0; i < branches.length; i++) {
    br = branches[i];
    var base_x = 0;
    var base_y = 0;
    var base_z = 0;
    if (i > 0) {
      base_x = branches[br.ConnectedTo - 1].TopX;
      base_y = branches[br.ConnectedTo - 1].TopY;
      base_z = branches[br.ConnectedTo - 1].TopZ;
    }
    quaternion_z.setFromAxisAngle(axis_z, br.VerAng);
    quaternion_y.setFromAxisAngle(axis_y, br.RotAng);
    branches_mat = br.Length / br.Diam1 < 10 ? mat_short : mat_long;
    const br_seg = branch_segment(
      br.Diam1,
      br.Diam2,
      br.Length,
      rad_seg(br.Diam1),
      branches_mat,
      is_branch_capped
    );
    br_seg.applyQuaternion(quaternion_z);
    br_seg.applyQuaternion(quaternion_y);
    br_seg.position.set(base_x, base_y, base_z);
    br_group.add(br_seg);
  }
  br_group.name = "branches";
  return br_group;
}

function generate_stump_obj(d, seg = 9, bark_type = "bark", bark_mat_short) {
  // const material = get_branches_material(bark_type, 0.3)
  const material = bark_mat_short;
  d = d * 0.9;
  const h = d * 2;
  const stumpg = new THREE.CylinderGeometry(d, d * 2, h, seg, 1, true);
  stumpg.translate(0, h / 2, 0);
  const rd = d * 0.7;
  const rl = d * 4;
  const rot_st = Math.PI * Math.random();
  const rot_div = (Math.PI * 2) / 3;
  const root = new THREE.ConeGeometry(rd, rl, 3, 1, true); //rad_seg(rd)
  root.translate(0, rl * 0.8, 0);
  root.rotateX(-Math.PI * 0.535);
  root.translate(0, rd * 0.5, 0);
  root.rotateY(Math.PI + rot_st);

  const root2 = root.clone();
  root2.rotateY(rot_div);
  const root3 = root2.clone();
  root3.rotateY(rot_div);

  const stumpcm = new THREE.Mesh(stumpg, material);
  const rootm = new THREE.Mesh(root, material);
  const rootm2 = new THREE.Mesh(root2, material);
  const rootm3 = new THREE.Mesh(root3, material);

  stumpcm.receiveShadow = true;
  rootm.receiveShadow = true;
  rootm2.receiveShadow = true;
  rootm3.receiveShadow = true;

  //----------------------------
  const stump = new THREE.Group();
  stump.add(stumpcm);
  stump.add(rootm);
  stump.add(rootm2);
  stump.add(rootm3);

  stump.name = "stump";
  return stump;
}

function generate_bush(scale = 0.3, plant_mats) {
  // const p = "plant_";
  const pg = new THREE.Group();
  for (let i = 1; i <= 7; i++) {
    const r = Math.random() * scale * 3;
    pg.add(generate_plant(plant_mats[i - 1], scale * 3 + r, scale + r));
  }
  pg.name = "bush";
  return pg;
}

function get_plant_material(tex_path = def_plants[0]) {
  const plant_tex = new THREE.TextureLoader().load(tex_path);
  const plant_mat = new THREE.MeshLambertMaterial({ map: plant_tex });
  plant_mat.side = THREE.DoubleSide;
  plant_mat.transparent = true;
  plant_mat.alphaTest = 0.5;
  return plant_mat;
}

function generate_plant(plant_mat, scale = 1, random_radius = 1) {
  const p1 = new THREE.PlaneGeometry(scale, scale);
  const p2 = p1.clone();
  p2.rotateY(Math.PI / 2);
  const p1m = new THREE.Mesh(p1, plant_mat);
  p1m.castShadow = true;
  p1m.receiveShadow = true;
  const p2m = new THREE.Mesh(p2, plant_mat);
  p2m.castShadow = true;
  p2m.receiveShadow = true;
  const pg = new THREE.Group();
  pg.add(p1m);
  pg.add(p2m);
  pg.translateY(scale / 2);
  var t = random_radius + Math.random() * random_radius;
  var r = 2 * Math.PI * Math.random();
  pg.translateX(t * Math.cos(r));
  pg.translateZ(t * Math.sin(r));
  return pg;
}

function generate_leaves_obj(leaves_pos, crown_radius, leaves_mat) {
  const quaternion_z = new THREE.Quaternion();
  const quaternion_y = new THREE.Quaternion();
  const quaternion_x = new THREE.Quaternion();
  const axis_z = new THREE.Vector3(0, 0, 1);
  const axis_y = new THREE.Vector3(0, 1, 0);
  const axis_x = new THREE.Vector3(1, 0, 0);

  const triangles = Math.floor(crown_radius * 12);
  const geometry = new THREE.BufferGeometry();
  const positions = [];
  const normals = [];
  const uvs = [];
  const textureIndices = [];

  const pd = crown_radius / 3; //position deviation radius
  const d = crown_radius / 6; // individual triangle leave size
  const n_vector = new THREE.Vector3();
  const v = new THREE.Vector3();

  const pA = new THREE.Vector3();
  const pB = new THREE.Vector3();
  const pC = new THREE.Vector3();

  const cb = new THREE.Vector3();
  const ab = new THREE.Vector3();

  for (var i = 0; i < leaves_pos.length; i++) {
    lv = leaves_pos[i];
    for (let i = 0; i < triangles; i++) {
      quaternion_z.setFromAxisAngle(axis_z, Math.PI * Math.random());
      quaternion_y.setFromAxisAngle(axis_y, Math.PI * Math.random());
      quaternion_x.setFromAxisAngle(axis_x, Math.PI * Math.random());

      // positions
      const xr = lv.x + (0.5 - Math.random()) * pd;
      const yr = lv.y + (0.5 - Math.random()) * pd;
      const zr = lv.z + (0.5 - Math.random()) * pd;

      const x = -d / 3;
      const y = -d / 3;
      const z = 0;

      // random rotation
      v.set(x, y, z);
      v.applyQuaternion(quaternion_z);
      v.applyQuaternion(quaternion_y);
      v.applyQuaternion(quaternion_x);

      const ax = v.x + xr;
      const ay = v.y + yr;
      const az = v.z + zr;

      v.set(x, y + d, z);
      v.applyQuaternion(quaternion_z);
      v.applyQuaternion(quaternion_y);
      v.applyQuaternion(quaternion_x);
      const bx = v.x + xr;
      const by = v.y + yr;
      const bz = v.z + zr;

      v.set(x + d, y, z);
      v.applyQuaternion(quaternion_z);
      v.applyQuaternion(quaternion_y);
      v.applyQuaternion(quaternion_x);
      const cx = v.x + xr;
      const cy = v.y + yr;
      const cz = v.z + zr;

      positions.push(ax, ay, az);
      positions.push(bx, by, bz);
      positions.push(cx, cy, cz);

      // normals
      // n_vector.set(xr, yr, zr);
      // n_vector.normalize();
      // const nx = n_vector.x;
      // const ny = n_vector.y;
      // const nz = n_vector.z;
      // normals.push(nx, ny, nz);
      // normals.push(nx, ny, nz);
      // normals.push(nx, ny, nz);

      // flat face normals

      pA.set(ax, ay, az);
      pB.set(bx, by, bz);
      pC.set(cx, cy, cz);

      cb.subVectors(pC, pB);
      ab.subVectors(pA, pB);
      cb.cross(ab);

      cb.normalize();

      const nx = cb.x;
      const ny = cb.y;
      const nz = cb.z;

      normals.push(nx, ny, nz);
      normals.push(nx, ny, nz);
      normals.push(nx, ny, nz);

      // uvs
      uvs.push(0, 0);
      uvs.push(0, 1);
      uvs.push(1, 0);

      // texture indices
      const t = i % 3;
      textureIndices.push(t, t, t);
    }
  }

  function disposeArray() {
    this.array = null;
  }

  geometry.setAttribute(
    "position",
    new THREE.Float32BufferAttribute(positions, 3).onUpload(disposeArray)
  );
  geometry.setAttribute(
    "normal",
    new THREE.Float32BufferAttribute(normals, 3).onUpload(disposeArray)
  );
  geometry.setAttribute("uv", new THREE.Float32BufferAttribute(uvs, 2));
  geometry.setAttribute(
    "textureIndex",
    new THREE.Int16BufferAttribute(textureIndices, 1)
  );
  const leaves_mesh = new THREE.Mesh(geometry, leaves_mat);
  leaves_mesh.castShadow = true;
  leaves_mesh.receiveShadow = true;
  return leaves_mesh;
}

function generate_crown(vertices, indices, color) {
  const crown_geom = new THREE.BufferGeometry();
  crown_geom.setIndex(indices);
  crown_geom.setAttribute("position", new THREE.BufferAttribute(vertices, 3));
  const crown_mat = new THREE.MeshBasicMaterial({ color: color });
  crown_mat.wireframe = true;
  const crown_mesh = new THREE.Mesh(crown_geom, crown_mat);
  crown_mesh.rotation.x = -Math.PI / 2;
  return crown_mesh;
}

function generate_tree_obj(
  tree_def,
  bark_mat_long,
  bark_mat_short,
  leaves_mat,
  plant_mats
) {
  const t_group = new THREE.Group();
  //---------- Trunk and Branches ------------------------
  const branches = HTMLWidgets.dataframeToD3(tree_def.branches);
  // const branches_mat = get_branches_material("bark");
  const bark_type = "bark";
  const branches_obj = generate_branches_obj(
    branches,
    bark_type,
    bark_mat_long,
    bark_mat_short,
    false
  );
  t_group.add(branches_obj);

  const stump = new generate_stump_obj(
    tree_def.dbh / 100,
    rad_seg(tree_def.dbh / 100),
    bark_type,
    bark_mat_short
  );
  t_group.add(stump);

  if (tree_def.dbh > 15) {
    const bush = new generate_bush(tree_def.dbh / 100, plant_mats);
    t_group.add(bush);
  }

  //---------- Leaves ------------------------
  const crown_radius = tree_def.crown_radius_max;
  const leaves_pos = HTMLWidgets.dataframeToD3(tree_def.leaves);
  // const leaves_mat = get_leaves_material("leaves_1");
  const leaves_obj = generate_leaves_obj(leaves_pos, crown_radius, leaves_mat);
  leaves_obj.name = "leaves";
  t_group.add(leaves_obj);
  //---------- Crown ------------------------
  const vertices = new Float32Array(tree_def.vertices);
  const indices = tree_def.indices;
  crown_mesh = generate_crown(vertices, indices, tree_def.crown_color);
  crown_mesh.name = "crown";
  crown_mesh.visible = false;
  t_group.add(crown_mesh);

  return t_group;
}

function vertexShader() {
  return `
    varying vec3 vWorldPosition;

    void main() {

      vec4 worldPosition = modelMatrix * vec4( position, 1.0 );
      vWorldPosition = worldPosition.xyz;

      gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );

    }
  `;
}

function fragmentShader() {
  return `
    uniform vec3 topColor;
    uniform vec3 bottomColor;
    uniform float offset;
    uniform float exponent;

    varying vec3 vWorldPosition;

    void main() {

      float h = normalize( vWorldPosition + offset ).y;
      gl_FragColor = vec4( mix( bottomColor, topColor, max( pow( max( h , 0.0), exponent ), 0.0 ) ), 1.0 );

    }
  `;
}

function set_scene_env(
  scene,
  ground_tex_path = def_ground_path,
  ground_bump_path = def_ground_bump_path
) {
  scene.background = new THREE.Color().setHSL(0.6, 0, 1);
  scene.fog = new THREE.Fog(scene.background, 1, 3000);

  // LIGHTS

  const hemiLight = new THREE.HemisphereLight(0xffffff, 0xffffff, 1);
  hemiLight.color.setHSL(0.6, 1, 0.6);
  hemiLight.groundColor.setHSL(0.095, 1, 0.75);
  hemiLight.position.set(0, 50, 0);
  scene.add(hemiLight);

  const dirLight = new THREE.DirectionalLight(0xffffff, 1.5);
  dirLight.color.setHSL(0.1, 1, 0.95);
  // dirLight.position.set( - 1, 1.75, 1 );
  dirLight.position.set(-0.8, 2, 0.8);
  dirLight.position.multiplyScalar(50);

  scene.add(dirLight);

  dirLight.castShadow = true;

  dirLight.shadow.mapSize.width = 1024; //2048;
  dirLight.shadow.mapSize.height = 1024; //2048;

  const d = 100;

  dirLight.shadow.camera.left = -d;
  dirLight.shadow.camera.right = d;
  dirLight.shadow.camera.top = d;
  dirLight.shadow.camera.bottom = -d;

  dirLight.shadow.camera.far = 3500;
  dirLight.shadow.bias = -0.0001;

  // GROUND

  // const texture = new THREE.TextureLoader().load("lib/threejs-156/ground.png");
  const texture = new THREE.TextureLoader().load(ground_tex_path);
  texture.wrapS = THREE.RepeatWrapping;
  texture.wrapT = THREE.RepeatWrapping;
  texture.repeat.set(1000, 1000);
  // const textbump = new THREE.TextureLoader().load("lib/threejs-156/ground_bump.png");
  const textbump = new THREE.TextureLoader().load(ground_bump_path);
  textbump.wrapS = THREE.RepeatWrapping;
  textbump.wrapT = THREE.RepeatWrapping;
  textbump.repeat.set(1000, 1000);

  const groundGeo = new THREE.PlaneGeometry(5000, 5000);
  const groundMat = new THREE.MeshLambertMaterial({ map: texture });
  groundMat.bumpMap = textbump;
  groundMat.bumpScale = 0.8;

  // const groundMat = new THREE.MeshLambertMaterial( { color: 0xffffff, map: texture} );
  // groundMat.color.setHSL( 0.095, 1, 0.75 );

  const ground = new THREE.Mesh(groundGeo, groundMat);
  ground.rotation.x = -Math.PI / 2;
  ground.receiveShadow = true;
  // scene.add( ground );

  // SKYDOME
  const uniforms = {
    topColor: { value: new THREE.Color(0x0077ff) },
    bottomColor: { value: new THREE.Color(0xffffff) },
    offset: { value: 33 },
    exponent: { value: 0.6 },
  };

  uniforms["topColor"].value.copy(hemiLight.color);

  scene.fog.color.copy(uniforms["bottomColor"].value);

  const skyGeo = new THREE.SphereGeometry(2000, 32, 15);
  const skyMat = new THREE.ShaderMaterial({
    uniforms: uniforms,
    vertexShader: vertexShader(),
    fragmentShader: fragmentShader(),
    side: THREE.BackSide,
  });

  const sky = new THREE.Mesh(skyGeo, skyMat);
  // scene.add( sky );

  const bg_group = new THREE.Group();
  bg_group.add(ground);
  bg_group.add(sky);
  scene.add(bg_group);
  return bg_group;
}

function generate_ground_plot(
  width = 10,
  height = 10,
  big_grid = 0,
  small_grid = 1,
  color_border = "#780000",
  color_big = "#344e41",
  color_small = "#d9d9d9"
) {
  color_border = new THREE.Color(color_border);
  color_small = new THREE.Color(color_small);
  color_big = new THREE.Color(color_big);

  const divisions_x = Math.ceil(width / small_grid);
  const divisions_z = Math.ceil(height / small_grid);
  const halfwidth = width / 2;
  const halfheight = height / 2;

  const vertices = [],
    colors = [];

  var color;
  for (
    let i = 0, j = 0, k = -halfwidth;
    i <= divisions_x;
    i++, k += small_grid
  ) {
    vertices.push(k, 0, -halfheight, k, 0, halfheight);
    color =
      i == 0 || i == divisions_x
        ? color_border
        : big_grid > 0 && (i * small_grid) % big_grid == 0
        ? color_big
        : color_small;
    color.toArray(colors, j);
    j += 3;
    color.toArray(colors, j);
    j += 3;
  }

  for (
    let i = 0, j = colors.length, k = -halfheight;
    i <= divisions_z;
    i++, k += small_grid
  ) {
    vertices.push(-halfwidth, 0, k, halfwidth, 0, k);
    color =
      i == 0 || i == divisions_z
        ? color_border
        : big_grid > 0 && (i * small_grid) % big_grid == 0
        ? color_big
        : color_small;
    color.toArray(colors, j);
    j += 3;
    color.toArray(colors, j);
    j += 3;
  }

  const geometry = new THREE.BufferGeometry();
  geometry.setAttribute(
    "position",
    new THREE.Float32BufferAttribute(vertices, 3)
  );
  geometry.setAttribute("color", new THREE.Float32BufferAttribute(colors, 3));
  const material = new THREE.LineBasicMaterial({
    vertexColors: true,
    toneMapped: false,
  });
  material.transparent = true;
  material.opacity = 0.5;
  const gp = new THREE.LineSegments(geometry, material);
  return gp;
}
