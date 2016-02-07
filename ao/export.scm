(define-module (ao export))

(use-modules (ao jit) (ao lib))

(define-public (ao-export-heightmap shape file X Y Z res)
    " Renders a shape and saves it to an image.
X, Y, Z are '(min max) intervals; res is a resolution in voxels per unit."
    (tree_export_heightmap (jit shape) file X Y Z res))

(define-public (ao-export-mesh shape file X Y Z res)
    " Renders a shape and saves it to an mesh
X, Y, Z are '(min max) intervals; res is a resolution in voxels per unit."
    (tree_export_mesh (jit shape) file X Y Z res))
