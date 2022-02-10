submodule (mainModuleOvertopping) submAllocOvertopping
contains
module procedure initGeometries
      type (tpCoordinatePair)         :: coordinates         !< vector with x/y-coordinates

      coordinates%N = geometryF%npoints
      coordinates%x = geometryF%xcoords
      coordinates%y = geometryF%ycoords
      call initializeGeometry(geometryF%normal, coordinates, geometryF%roughness, geometry, error)
      call setupGeometries(geometry%parent)
end procedure initGeometries

module procedure setupGeometries
      allocate(geometries%adjWithDikeHeight)
      allocate(geometries%geometryMergedBerms)
      allocate(geometries%geometrySectionB)
      allocate(geometries%geometrySectionF)
      allocate(geometries%geometryFlatBerms)
      allocate(geometries%geometryNoBerms(2))
      allocate(geometries%geometryRemoveDikeSegments)
      geometries%adjWithDikeHeight%parent => geometries
      geometries%geometrySectionB%parent => geometries
      geometries%geometrySectionF%parent => geometries
      geometries%geometryFlatBerms%parent => geometries
      geometries%geometryRemoveDikeSegments%parent => geometries
end procedure setupGeometries

module procedure cleanupGeometry
      call deallocateGeometry(geometries%adjWithDikeHeight)
      call deallocateGeometry(geometries%geometryMergedBerms)
      call deallocateGeometry(geometries%geometrySectionB)
      call deallocateGeometry(geometries%geometrySectionF)
      call deallocateGeometry(geometries%geometryFlatBerms)
      call deallocateGeometry(geometries%geometryNoBerms(1))
      call deallocateGeometry(geometries%geometryNoBerms(2))
      call deallocateGeometry(geometries%geometryRemoveDikeSegments)

      deallocate(geometries%adjWithDikeHeight)
      deallocate(geometries%geometryMergedBerms)
      deallocate(geometries%geometrySectionB)
      deallocate(geometries%geometrySectionF)
      deallocate(geometries%geometryFlatBerms)
      deallocate(geometries%geometryNoBerms)
      deallocate(geometries%geometryRemoveDikeSegments)

      call cleanupCoordinatePair(geometries%CoordsAdjusted)

end procedure cleanupGeometry

end submodule submAllocOvertopping
