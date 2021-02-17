#pragma once
#include "libfive.h"

// These are all markers used by the parser
#define LIBFIVE_STDLIB libfive_tree
#define LIBFIVE_SECTION(a)

#define vec2 libfive_vec2
#define vec3 libfive_vec3

#ifdef __cplusplus
extern "C" {
#endif

////////////////////////////////////////////////////////////////////////////////
LIBFIVE_SECTION(csg)

LIBFIVE_STDLIB _union(
    // Returns the union of two shapes
    libfive_tree a, libfive_tree b);
LIBFIVE_STDLIB intersection(
    // Returns the intersection of two shapes
    libfive_tree a, libfive_tree b);
LIBFIVE_STDLIB inverse(
    // Returns a shape that's the inverse of the input shape
    libfive_tree a);
LIBFIVE_STDLIB difference(
    // Subtracts the second shape from the first
    libfive_tree a, libfive_tree b);
LIBFIVE_STDLIB offset(
    // Expand or contract a given shape by an offset
    // Positive offsets expand the shape; negative offsets shrink it
    libfive_tree a, float o);
LIBFIVE_STDLIB clearance(
    // Expands shape b by the given offset then subtracts it from shape a
    libfive_tree a, libfive_tree b, float offset);
LIBFIVE_STDLIB shell(
    // Returns a shell of a shape with the given offset
    libfive_tree a, float offset);
LIBFIVE_STDLIB blend_expt(
    // Blends two shapes by the given amount using exponents
    libfive_tree a, libfive_tree b, float m);
LIBFIVE_STDLIB blend_expt_unit(
    // Blends two shapes by the given amount using exponents,
    // with the blend term adjusted to produce results approximately
    // resembling blend_rough for values between 0 and 1.
    libfive_tree a, libfive_tree b, float m);
LIBFIVE_STDLIB blend_rough(
    // Blends two shapes by the given amount, using a fast-but-rough
    // CSG approximation that may not preserve gradients
    libfive_tree a, libfive_tree b, float m);
LIBFIVE_STDLIB blend(
    // Alias for blend_expt_unit
    libfive_tree a, libfive_tree b, float m);
LIBFIVE_STDLIB blend_difference(
    // Blends the subtraction of b, with optional offset o,
    // from a, with smoothness m
    libfive_tree a, libfive_tree b,
    float m, float o__0);
LIBFIVE_STDLIB morph(
    // Morphs between two shapes.
    // m = 0 produces a, m = 1 produces b
    libfive_tree a, libfive_tree b, float m);
LIBFIVE_STDLIB loft(
    // Produces a blended loft between a (at zmin) and b (at zmax)
    // a and b should be 2D shapes (i.e. invariant along the z axis)
    libfive_tree a, libfive_tree b, float zmin, float zmax);
LIBFIVE_STDLIB loft_between(
    // Produces a blended loft between a (at lower.z) and b (at upper.z),
    // with XY coordinates remapped to slide between lower.xy and upper.xy.
    // a and b should be 2D shapes (i.e. invariant along the z axis)
    libfive_tree a, libfive_tree b, vec3 lower, vec3 upper);

////////////////////////////////////////////////////////////////////////////////
LIBFIVE_SECTION(shapes)

LIBFIVE_STDLIB circle(float r, vec2 center__0);
LIBFIVE_STDLIB ring(float ro, float ri, vec2 center__0);
LIBFIVE_STDLIB polygon(float r, int n, vec2 center__0);
LIBFIVE_STDLIB rectangle(vec2 a, vec2 b);

////////////////////////////////////////////////////////////////////////////////
LIBFIVE_SECTION(transforms)

LIBFIVE_STDLIB move(libfive_tree t, vec3 offset);

#ifdef __cplusplus
}
#endif
