(define-module (ao export))

(use-modules (ao jit) (ao lib))

(define-public (ao-export-heightmap shape file a b res)
    " Renders a shape and saves it to an image.
a and b are '(x y z) corners; res is a resolution in voxels per unit."
    (tree_export_heightmap (jit shape) file
        (cons (car   a) (car   b))
        (cons (cadr  a) (cadr  b))
        (cons (if (>= (length a) 3) (caddr a) 0)
              (if (>= (length b) 3) (caddr b) 0)) res))

(define-public (ao-export-mesh shape file a b res)
    " Renders a shape and saves it to an mesh
a and b are '(x y z) corners; res is a resolution in voxels per unit."
    (tree_export_mesh (jit shape) file
        (cons (car   a) (car   b))
        (cons (cadr  a) (cadr  b))
        (cons (caddr a)
              (caddr b)) res))
