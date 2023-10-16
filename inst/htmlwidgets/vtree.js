HTMLWidgets.widget({
  name: "vtree",

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
        var area = { x_length: 100, y_length: 100 };
        if (x.trees.dbh == undefined) {
          if (x.trees.area != undefined) {
            area = x.trees.area;
          }
          for (const [key, value] of Object.entries(x.trees)) {
            if (key != "area") plant_trees(value);
          }
        } else {
          plant_trees(x.trees);
        }

        function plant_trees(t) {
          const tobj = generate_tree_obj(
            t,
            mat_long,
            mat_short,
            leaves_mat,
            plant_mats
          );
          const px = t.x == undefined ? 0 : t.x - area.x_length / 2;
          const py = t.y == undefined ? 0 : t.y - area.y_length / 2;
          tobj.position.set(px, 0, py);
          scene.add(tobj);
        }

        if (x.setting.show_crown != null) {
          tree_obj.getObjectByName("crown").visible = x.setting.show_crown;
        }

        //---------- Background -------------
        const background_env = set_scene_env(scene);

        //---------- Plot ground ------------------------
        const grid = generate_ground_plot(area.x_length, area.y_length, 10, 1);
        grid.position.y = 0.01;
        scene.add(grid);

        //---------- View ------------------------
        camera = new THREE.PerspectiveCamera(
          50,
          window.innerWidth / window.innerHeight,
          1,
          5000
        );
        camera.position.set(0, 20, 100);

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
