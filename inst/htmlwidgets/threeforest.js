HTMLWidgets.widget({
  name: "threeforest",

  type: "output",

  factory: function (el, width, height) {
    const renderer = new THREE.WebGLRenderer({ antialias: true });
    // this.renderer = new THREE.WebGLRenderer({
    //   antialias: AA,
    //   powerPreference: "high-performance",
    // })
    const scene = new THREE.Scene();
    var camera;
    return {
      renderValue: function (x) {

        //---------- Texture ------------------------
        const bark_type = "bark";
        const mat_long = get_branches_material(bark_type, 3);
        const mat_short = get_branches_material(bark_type, 0.5);
        const leaves_mat = get_leaves_material("leaves_1");
        const plant_mats = [];
        const p = "plant_";
        for (let i = 0; i < 7; i++) {
          plant_mats.push(get_plant_material(def_plants[i]));
        }

        //---------- Tree ------------------------
        var area = [100, 100];
        if (x.forest.dbh == undefined) {
          if (x.forest.area != undefined) {
            area = x.forest.area;
          }
          for (const [key, value] of Object.entries(x.forest)) {
            if (key != "area") plant_trees(value);
          }
        } else {
          plant_trees(x.forest);
        }

        function plant_trees(t) {
          const tobj = generate_tree_obj(
            t,
            mat_long,
            mat_short,
            leaves_mat,
            plant_mats
          );
          const px = t.x == undefined ? 0 : t.x - area[0] / 2;
          const py = t.y == undefined ? 0 : t.y - area[1] / 2;
          tobj.position.set(px, 0, py);
          scene.add(tobj);
        }

        if (x.setting.show_crown != null) {
          // tree_obj.getObjectByName("crown").visible = x.setting.show_crown;
        }

        //---------- Background -------------
        const background_env = set_scene_env(scene);

        //---------- Plot ground ------------------------
        const grid = generate_ground_plot(area[0], area[1], 10, 1);
        grid.position.y = 0.01;
        scene.add(grid);

        //---------- View ------------------------
        camera = new THREE.PerspectiveCamera(
          50,
          window.innerWidth / window.innerHeight,
          1,
          5000
        );
        var camdist = 100;
        if (x.setting.camera_dist != null) {
          camdist = x.setting.camera_dist;
        }
        camera.position.set(0, 20, camdist);

        renderer.shadowMap.enabled = true;
        renderer.setSize(width, height);
        // while (el.hasChildNodes()) {
        //   el.removeChild(el.lastChild);
        // }
        el.appendChild(renderer.domElement);

        // var stats = new Stats();
        // stats.showPanel(1);
        // el.appendChild(stats.dom);

        //---------- Control ------------------------
        controls = new THREE.OrbitControls(camera, el);
        controls.maxPolarAngle = Math.PI / 2;
        controls.autoRotate = true;
        controls.minDistance = 2;
        controls.maxDistance = 200;
        controls.update();

        function animate() {
          requestAnimationFrame(animate);
          // stats.begin();
          controls.update();
          renderer.render(scene, camera);
          // stats.end();
        }
        animate();

        //--------- Setting UI --------------------
        const gui = new GUI();
        gui.close();
        gui.title("Setting");
        gui.add(controls, "autoRotate").name("Auto rotate");
        // gui.add(tree_obj.getObjectByName("crown"), 'visible').name('Crown boundary');
        gui.add(grid, "visible").name("Plot grid");
        gui.add(background_env, "visible").name("Background");
      },

      resize: function (width, height) {
        renderer.setSize(width, height);
        camera.aspect = width / height;
        camera.updateProjectionMatrix();
      },

      scene: scene
    };
  },
});
