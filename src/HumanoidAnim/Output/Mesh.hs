-- |
-- Module      : HumanoidAnim.Output.Mesh
-- Description : Stick figure mesh generation for humanoid skeleton
--
-- This module generates a simple stick figure mesh that follows
-- the humanoid bone structure. Each bone is represented as a capsule
-- (cylinder with rounded ends), and joints are represented as spheres.
module HumanoidAnim.Output.Mesh
  ( -- * Mesh Data Types
    MeshData(..)
  , VertexData(..)

    -- * Mesh Generation
  , generateStickFigure
  , generateBoneCapsule
  , generateJointSphere

    -- * Configuration
  , MeshConfig(..)
  , defaultMeshConfig
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Word (Word16)
import Linear (V3(..), V4(..), normalize, cross, (^-^), (^+^), (*^), dot, norm)

import HumanoidAnim.Skeleton.Bones
import HumanoidAnim.Skeleton.Config (Skeleton(..), Transform(..))
import HumanoidAnim.Skeleton.Hierarchy (boneParent)

-- | Mesh configuration
data MeshConfig = MeshConfig
  { meshBoneRadius :: Float
    -- ^ Radius of bone capsules (default: 0.02m)
  , meshJointRadius :: Float
    -- ^ Radius of joint spheres (default: 0.03m)
  , meshSegments :: Int
    -- ^ Number of segments around circumference (default: 8)
  , meshRingsPerBone :: Int
    -- ^ Number of rings along bone length (default: 4)
  } deriving stock (Show, Eq)

-- | Default mesh configuration
defaultMeshConfig :: MeshConfig
defaultMeshConfig = MeshConfig
  { meshBoneRadius = 0.02
  , meshJointRadius = 0.03
  , meshSegments = 8
  , meshRingsPerBone = 4
  }

-- | Vertex data with position, normal, and bone weights
data VertexData = VertexData
  { vertPosition :: V3 Float
  , vertNormal :: V3 Float
  , vertJoints :: V4 Word16      -- Up to 4 bone influences
  , vertWeights :: V4 Float      -- Corresponding weights
  } deriving stock (Show, Eq)

-- | Complete mesh data
data MeshData = MeshData
  { meshVertices :: [VertexData]
  , meshIndices :: [Word16]
  , meshBoneIndices :: [Int]     -- Bone index for each vertex (for skinning)
  } deriving stock (Show, Eq)

-- | Generate stick figure mesh for skeleton
generateStickFigure :: MeshConfig -> Skeleton -> MeshData
generateStickFigure cfg skeleton = combineMeshes allMeshes
  where
    restPose = skeletonRestPose skeleton
    boneLengths = skeletonLengths skeleton
    activeBones = Set.toList (skeletonActiveBones skeleton)

    -- Create bone index lookup
    boneIndexMap :: Map HumanoidBone Int
    boneIndexMap = Map.fromList (zip activeBones [0..])

    -- Generate meshes for each bone connection and joint
    allMeshes = jointMeshes ++ boneMeshes

    -- Generate spheres at each joint
    jointMeshes =
      [ generateJointSphere cfg pos boneIdx
      | bone <- activeBones
      , let boneIdx = Map.findWithDefault 0 bone boneIndexMap
      , Just (Transform pos _) <- [Map.lookup bone restPose]
      ]

    -- Generate capsules for each bone segment
    boneMeshes =
      [ generateBoneCapsule cfg startPos endPos parentIdx childIdx
      | childBone <- activeBones
      , Just parentBone <- [boneParent childBone]
      , parentBone `Set.member` skeletonActiveBones skeleton
      , let parentIdx = Map.findWithDefault 0 parentBone boneIndexMap
      , let childIdx = Map.findWithDefault 0 childBone boneIndexMap
      , Just (Transform startPos _) <- [Map.lookup parentBone restPose]
      , Just (Transform endPos _) <- [Map.lookup childBone restPose]
      ]

-- | Generate a sphere at a joint position
generateJointSphere :: MeshConfig -> V3 Float -> Int -> MeshData
generateJointSphere cfg center boneIdx = MeshData
  { meshVertices = vertices
  , meshIndices = indices
  , meshBoneIndices = replicate (length vertices) boneIdx
  }
  where
    r = meshJointRadius cfg
    segs = meshSegments cfg
    rings = segs `div` 2  -- Vertical rings

    -- Generate vertices for UV sphere
    vertices =
      [ VertexData pos normal (V4 (fromIntegral boneIdx) 0 0 0) (V4 1 0 0 0)
      | ring <- [0..rings]
      , seg <- [0..segs-1]
      , let phi = pi * fromIntegral ring / fromIntegral rings
      , let theta = 2 * pi * fromIntegral seg / fromIntegral segs
      , let x = sin phi * cos theta
      , let y = cos phi
      , let z = sin phi * sin theta
      , let normal = V3 x y z
      , let pos = center ^+^ (r *^ normal)
      ]

    -- Generate indices for triangles
    indices = concat
      [ if ring < rings
        then [ fromIntegral $ ring * segs + seg
             , fromIntegral $ ring * segs + nextSeg
             , fromIntegral $ (ring + 1) * segs + seg
             , fromIntegral $ (ring + 1) * segs + seg
             , fromIntegral $ ring * segs + nextSeg
             , fromIntegral $ (ring + 1) * segs + nextSeg
             ]
        else []
      | ring <- [0..rings-1]
      , seg <- [0..segs-1]
      , let nextSeg = (seg + 1) `mod` segs
      ]

-- | Generate a capsule (cylinder) between two joints
generateBoneCapsule :: MeshConfig -> V3 Float -> V3 Float -> Int -> Int -> MeshData
generateBoneCapsule cfg start end parentIdx childIdx = MeshData
  { meshVertices = vertices
  , meshIndices = indices
  , meshBoneIndices = boneIndices
  }
  where
    r = meshBoneRadius cfg
    segs = meshSegments cfg
    rings = meshRingsPerBone cfg

    -- Bone direction and length
    dir = end ^-^ start
    len = norm dir
    ndir = if len > 0.001 then normalize dir else V3 0 1 0

    -- Create perpendicular vectors for cylinder
    up = if abs (dot ndir (V3 0 1 0)) > 0.99
         then V3 1 0 0
         else V3 0 1 0
    tangent = normalize (cross ndir up)
    bitangent = cross ndir tangent

    -- Generate vertices along the cylinder
    vertices =
      [ VertexData pos normal joints weights
      | ring <- [0..rings]
      , seg <- [0..segs-1]
      , let t = fromIntegral ring / fromIntegral rings  -- 0 to 1 along bone
      , let theta = 2 * pi * fromIntegral seg / fromIntegral segs
      , let cx = cos theta
      , let cz = sin theta
      , let normal = normalize (cx *^ tangent ^+^ cz *^ bitangent)
      , let ringCenter = start ^+^ (t * len) *^ ndir
      , let pos = ringCenter ^+^ (r *^ normal)
      -- Weight: blend from parent (1-t) to child (t)
      , let parentWeight = 1 - t
      , let childWeight = t
      , let joints = V4 (fromIntegral parentIdx) (fromIntegral childIdx) 0 0
      , let weights = V4 parentWeight childWeight 0 0
      ]

    -- Bone index follows weight (majority bone)
    boneIndices =
      [ if t < 0.5 then parentIdx else childIdx
      | ring <- [0..rings]
      , _ <- [0..segs-1]
      , let t = fromIntegral ring / fromIntegral rings :: Float
      ]

    -- Generate indices for cylinder triangles
    indices = concat
      [ if ring < rings
        then [ fromIntegral $ ring * segs + seg
             , fromIntegral $ (ring + 1) * segs + seg
             , fromIntegral $ ring * segs + nextSeg
             , fromIntegral $ ring * segs + nextSeg
             , fromIntegral $ (ring + 1) * segs + seg
             , fromIntegral $ (ring + 1) * segs + nextSeg
             ]
        else []
      | ring <- [0..rings-1]
      , seg <- [0..segs-1]
      , let nextSeg = (seg + 1) `mod` segs
      ]

-- | Combine multiple meshes into one
combineMeshes :: [MeshData] -> MeshData
combineMeshes meshes = foldl' addMesh emptyMesh meshes
  where
    emptyMesh = MeshData [] [] []

    addMesh :: MeshData -> MeshData -> MeshData
    addMesh acc mesh = MeshData
      { meshVertices = meshVertices acc ++ meshVertices mesh
      , meshIndices = meshIndices acc ++
          map (+ fromIntegral (length (meshVertices acc))) (meshIndices mesh)
      , meshBoneIndices = meshBoneIndices acc ++ meshBoneIndices mesh
      }
