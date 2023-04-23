﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module LightMap =

    /// Create a reflection map.
    let CreateReflectionMap (render, currentViewport : Viewport, currentRenderbuffer, currentFramebuffer, origin, renderbuffer, framebuffer) =

        // construct viewport
        let viewport =
            Viewport
                (Constants.Render.NearPlaneDistanceOmnipresent,
                 Constants.Render.FarPlaneDistanceOmnipresent,
                 box2i v2iZero Constants.Render.Resolution)

        // construct eye rotations
        let eyeRotations =
            [|(v3Right, v3Down)
              (v3Left, v3Down)
              (v3Up, v3Back)
              (v3Down, v3Forward)
              (v3Back, v3Down)
              (v3Forward, v3Down)|]

        // construct projection
        let projection = Matrix4x4.CreatePerspectiveFieldOfView (MathHelper.PiOver2, viewport.AspectRatio, viewport.NearDistance, viewport.FarDistance)

        // create cube map to render reflection to
        let reflectionMap = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, reflectionMap)
        Hl.Assert ()

        // setup buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // mutate viewport
        Gl.Viewport (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, viewport.Bounds.Width, viewport.Bounds.Height)
        Hl.Assert ()

        // render reflection map faces
        for i in 0 .. dec 6 do

            // construct face view, projection, and viewport
            let (eyeForward, eyeUp) = eyeRotations.[i]
            let viewAbsolute = m4Identity
            let viewRelative = Matrix4x4.CreateLookAt (origin, origin + eyeForward, eyeUp)
            let viewSkyBox = Matrix4x4.Transpose (Matrix4x4.CreateLookAt (v3Zero, eyeForward, eyeUp)) // transpose = inverse rotation when rotation only

            // select cube map face for second pass
            let proceedToSecondPass _ geometryFramebuffer =

                // set up cube face for rendering to
                let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
                Gl.TexImage2D (target, 0, InternalFormat.Rgba32f, viewport.Bounds.Width, viewport.Bounds.Height, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
                Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
                Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
                Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
                Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
                Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
                Hl.Assert ()
            
                // copy depths from geometry framebuffer to texture
                Gl.BindFramebuffer (FramebufferTarget.ReadFramebuffer, geometryFramebuffer)
                Gl.FramebufferTexture2D (FramebufferTarget.DrawFramebuffer, FramebufferAttachment.ColorAttachment0, target, reflectionMap, 0)
                Gl.BlitFramebuffer
                    (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, viewport.Bounds.Size.X, viewport.Bounds.Size.Y,
                     viewport.Bounds.Min.X, viewport.Bounds.Min.Y, viewport.Bounds.Size.X, viewport.Bounds.Size.Y,
                     ClearBufferMask.DepthBufferBit,
                     BlitFramebufferFilter.Nearest)
                Hl.Assert ()

                // switch to texture buffer
                Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, reflectionMap, 0)
                Hl.Assert ()

            // render to cube map face
            render proceedToSecondPass false origin viewAbsolute viewRelative viewSkyBox projection viewport renderbuffer framebuffer
            Hl.Assert ()

        // teardown viewport
        Gl.Viewport (currentViewport.Bounds.Min.X, currentViewport.Bounds.Min.Y, currentViewport.Bounds.Size.X, currentViewport.Bounds.Size.Y)
        Hl.Assert ()

        // teardown framebuffer
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, currentRenderbuffer)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, currentFramebuffer)
        reflectionMap

    let CreateIrradianceMap
        (currentViewport : Viewport,
         currentRenderbuffer,
         currentFramebuffer,
         renderbufferWidth,
         renderbufferHeight,
         renderbuffer,
         framebuffer,
         irradianceShader,
         cubeMapSurface : CubeMap.CubeMapSurface) =

        // create irradiance map
        let irradianceMap = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMap)
        Hl.Assert ()

        // setup irradiance cube map for rendering to
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.Rgba16f, renderbufferWidth, renderbufferHeight, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Hl.Assert ()

        // setup framebuffer
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // mutate viewport
        Gl.Viewport (0, 0, renderbufferWidth, renderbufferHeight)
        Hl.Assert ()

        // compute views and projection
        let views =
            [|(Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)).ToArray ()|]
        let projection = (Matrix4x4.CreatePerspectiveFieldOfView (MathHelper.PiOver2, 1.0f, 0.1f, 10.0f)).ToArray ()

        // render faces to irradiant map
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, irradianceMap, 0)
            CubeMap.DrawCubeMap (views.[i], projection, cubeMapSurface.CubeMap, cubeMapSurface.CubeMapGeometry, irradianceShader)
            Hl.Assert ()

        // teardown viewport
        Gl.Viewport (currentViewport.Bounds.Min.X, currentViewport.Bounds.Min.Y, currentViewport.Bounds.Size.X, currentViewport.Bounds.Size.Y)
        Hl.Assert ()

        // teardown framebuffer
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, currentRenderbuffer)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, currentFramebuffer)
        irradianceMap

    /// Describes a environment filter shader that's loaded into GPU.
    type EnvironmentFilterShader =
        { ViewUniform : int
          ProjectionUniform : int
          RoughnessUniform : int
          ResolutionUniform : int
          ColorUniform : int
          BrightnessUniform : int
          CubeMapUniform : int
          EnvironmentFilterShader : uint }

    /// Create a environment filter shader.
    let CreateEnvironmentFilterShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let roughnessUniform = Gl.GetUniformLocation (shader, "roughness")
        let resolutionUniform = Gl.GetUniformLocation (shader, "resolution")
        let colorUniform = Gl.GetUniformLocation (shader, "color")
        let brightnessUniform = Gl.GetUniformLocation (shader, "brightness")
        let cubeMapUniform = Gl.GetUniformLocation (shader, "cubeMap")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          RoughnessUniform = roughnessUniform
          ResolutionUniform = resolutionUniform
          ColorUniform = colorUniform
          BrightnessUniform = brightnessUniform
          CubeMapUniform = cubeMapUniform
          EnvironmentFilterShader = shader }

    /// Draw an environment filter.
    let DrawEnvironmentFilter
        (view : single array,
         projection : single array,
         roughness : single,
         resolution : single,
         cubeMap : uint,
         geometry : CubeMap.CubeMapGeometry,
         shader : EnvironmentFilterShader) =

        // setup shader
        Gl.UseProgram shader.EnvironmentFilterShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform1 (shader.RoughnessUniform, roughness)
        Gl.Uniform1 (shader.ResolutionUniform, resolution)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.CubeMapVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Gl.UseProgram 0u
        Hl.Assert ()

    let CreateEnvironmentFilterMap
        (currentViewport : Viewport,
         currentRenderbuffer,
         currentFramebuffer,
         renderbufferWidth,
         renderbufferHeight,
         renderbuffer,
         framebuffer,
         environmentFilterShader,
         environmentFilterSurface : CubeMap.CubeMapSurface) =

        // create environment filter map
        let environmentFilterMap = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, environmentFilterMap)
        Hl.Assert ()

        // setup environment filter cube map for rendering to
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.Rgba16f, renderbufferWidth, renderbufferHeight, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.LinearMipmapLinear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Gl.GenerateMipmap TextureTarget.TextureCubeMap
        Hl.Assert ()

        // setup framebuffer
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // compute views and projection
        let views =
            [|(Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)).ToArray ()|]
        let projection = (Matrix4x4.CreatePerspectiveFieldOfView (MathHelper.PiOver2, 1.0f, 0.1f, 10.0f)).ToArray ()

        // render environment filter map mips
        for i in 0 .. dec Constants.Render.EnvironmentFilterMips do
            let roughness = single i / single (dec Constants.Render.EnvironmentFilterMips)
            let resolution = single Constants.Render.EnvironmentFilterResolution * pown 0.5f i
            Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.DepthComponent24, int resolution, int resolution)
            Gl.Viewport (0, 0, int resolution, int resolution)
            for j in 0 .. dec 6 do
                let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + j)
                Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, environmentFilterMap, i)
                DrawEnvironmentFilter (views.[j], projection, roughness, resolution, environmentFilterSurface.CubeMap, environmentFilterSurface.CubeMapGeometry, environmentFilterShader)
                Hl.Assert ()

        // teardown viewport
        Gl.Viewport (currentViewport.Bounds.Min.X, currentViewport.Bounds.Min.Y, currentViewport.Bounds.Size.X, currentViewport.Bounds.Size.Y)
        Hl.Assert ()

        // teardown framebuffer
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, currentRenderbuffer)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, currentFramebuffer)
        environmentFilterMap

    /// A collection of maps consisting a light map.
    type [<StructuralEquality; NoComparison; Struct>] LightMap =
        { Origin : Vector3
          ReflectionMap : uint
          IrradianceMap : uint
          EnvironmentFilterMap : uint }

    /// Create a light map.
    let CreateLightMap origin irradianceMap environmentFilterMap reflectionMap =
        { Origin = origin
          ReflectionMap = reflectionMap
          IrradianceMap = irradianceMap
          EnvironmentFilterMap = environmentFilterMap }

    /// Destroy a light map.
    let DestroyLightMap lightMap =
        CubeMap.DeleteCubeMap lightMap.IrradianceMap
        CubeMap.DeleteCubeMap lightMap.EnvironmentFilterMap
        CubeMap.DeleteCubeMap lightMap.ReflectionMap