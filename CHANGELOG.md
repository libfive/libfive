# Changelog

## [Pre-release]
### Fixed
- In Scheme, `constant` did not behave correctly under remapping transforms.
- Fixed an issue with normals of expressions containing `sqrt`
### Added
- A few more shapes, like `rounded-rectangle-exact`
- Added `nan-fill` to Scheme interface.
- Added `Oracle` interface, for embedding arbitrary black-box functions in math expressions.
- Changes to how ambiguous features are handled in meshing (should have no user-visible impact).
- Better error messages in Studio, printing function names when available
- CHANGELOG (based on [keepachangelog.com](https://keepachangelog.com/en/1.0.0/))
