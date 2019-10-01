unit MyElement;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demo3dcube2}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava;

type

   { TCube }

   TCube = class
   private
     	vertexBuffer: JNFloatBuffer;   // The buffer holding the vertices
	textureBuffer: JNFloatBuffer;  // The buffer holding the texture coordinates
	indexBuffer: JNByteBuffer;     // The buffer holding the indices
	normalBuffer: JNFloatBuffer;   // The buffer holding the normals
        textures: array[0..2] of jint;   // Our texture pointer    //	private int[]  = new int[3];
     const
         // The initial vertex definition
        verticesLength = 71;
	vertices: array[0..verticesLength] of jfloat = ( //Vertices according to faces
			               -1.0, -1.0, 1.0,  //v0
			                1.0, -1.0, 1.0,  //v1
			               -1.0,  1.0, 1.0,  //v2
			                1.0,  1.0, 1.0,  //v3

			                1.0, -1.0,  1.0, // ...
			                1.0, -1.0, -1.0,
			                1.0,  1.0,  1.0,
			                1.0,  1.0, -1.0,

			                1.0, -1.0, -1.0,
			               -1.0, -1.0, -1.0,
			                1.0,  1.0, -1.0,
			               -1.0,  1.0, -1.0,

			               -1.0, -1.0, -1.0,
			               -1.0, -1.0,  1.0,
			               -1.0,  1.0, -1.0,
			               -1.0,  1.0,  1.0,

			               -1.0, -1.0, -1.0,
			                1.0, -1.0, -1.0,
			               -1.0, -1.0,  1.0,
			                1.0, -1.0,  1.0,

			               -1.0,  1.0,  1.0,
			                1.0,  1.0,  1.0,
			               -1.0,  1.0, -1.0,
			                1.0,  1.0, -1.0
			                );

        {
	 * The initial normals for the lighting calculations
	 *
	 * The normals are not necessarily correct from a
	 * real world perspective, as I am too lazy to write
	 * these all on my own. But you get the idea and see
	 * what I mean if you run the demo.
	 *
        }
        normalsLength = 71;
        normals: array [0..normalsLength] of jfloat = ( // Normals
				                      0.0,  0.0,  1.0,
				                      0.0,  0.0, -1.0,
				                      0.0,  1.0,  0.0,
				                      0.0, -1.0,  0.0,

				                      0.0,  0.0,  1.0,
				                      0.0,  0.0, -1.0,
				                      0.0,  1.0,  0.0,
				                      0.0, -1.0,  0.0,

				                      0.0,  0.0,  1.0,
				                      0.0,  0.0, -1.0,
				                      0.0,  1.0,  0.0,
				                      0.0, -1.0,  0.0,

				                      0.0,  0.0,  1.0,
				                      0.0,  0.0, -1.0,
				                      0.0,  1.0,  0.0,
				                      0.0, -1.0,  0.0,

				                      0.0,  0.0,  1.0,
				                      0.0,  0.0, -1.0,
				                      0.0,  1.0,  0.0,
				                      0.0, -1.0,  0.0,

				                      0.0,  0.0,  1.0,
				                      0.0,  0.0, -1.0,
				                      0.0,  1.0,  0.0,
				                      0.0, -1.0,  0.0
				                      );

	// The initial texture coordinates (u, v)
        textureLength = 47;
	texture: array[0..textureLength] of jfloat = (	//Mapping coordinates for the vertices
				      0.0, 0.0,
				      0.0, 1.0,
				      1.0, 0.0,
				      1.0, 1.0,

				      0.0, 0.0,
				      0.0, 1.0,
				      1.0, 0.0,
				      1.0, 1.0,

				      0.0, 0.0,
				      0.0, 1.0,
				      1.0, 0.0,
				      1.0, 1.0,

				      0.0, 0.0,
				      0.0, 1.0,
				      1.0, 0.0,
				      1.0, 1.0,

				      0.0, 0.0,
				      0.0, 1.0,
				      1.0, 0.0,
				      1.0, 1.0,

				      0.0, 0.0,
				      0.0, 1.0,
				      1.0, 0.0,
				      1.0, 1.0
				      );

	// The initial indices definition */
        indicesLength = 35;
	indices: array[0..indicesLength] of jbyte = (   // Faces definition
	                    0, 1, 3, 0, 3, 2, 		// Face front
	                    4, 5, 7, 4, 7, 6, 		// Face right
	                    8, 9, 11, 8, 11, 10, 	// ...
	                    12, 13, 15, 12, 15, 14,
	                    16, 17, 19, 16, 19, 18,
	                    20, 21, 23, 20, 23, 22
	                    );


   public
     constructor Create; virtual;
     procedure Draw(gl: JMKOGL10; filter: jint);
     procedure loadGLTexture(gl: JMKOGL10; Context: ACContext);
     procedure buildMipmap(gl: JMKOGL10; bitmap: AGBitmap);

   end;

implementation

{ TCube }

constructor TCube.Create;
var
  byteBuf: JNByteBuffer;
begin
  //
  byteBuf := JNByteBuffer.allocateDirect((verticesLength + 1) * 4);
  byteBuf.order(JNByteOrder.nativeOrder());
  vertexBuffer := byteBuf.asFloatBuffer();
  vertexBuffer.put(vertices);
  vertexBuffer.position(0);

  //
  byteBuf := JNByteBuffer.allocateDirect((textureLength + 1) * 4);
  byteBuf.order(JNByteOrder.nativeOrder());
  textureBuffer := byteBuf.asFloatBuffer();
  textureBuffer.put(texture);
  textureBuffer.position(0);

  //
  byteBuf := JNByteBuffer.allocateDirect((normalsLength + 1) * 4);
  byteBuf.order(JNByteOrder.nativeOrder());
  normalBuffer := byteBuf.asFloatBuffer();
  normalBuffer.put(normals);
  normalBuffer.position(0);

  //
  indexBuffer := JNByteBuffer.allocateDirect(indicesLength + 1);
  indexBuffer.put(indices);
  indexBuffer.position(0);
end;

procedure TCube.Draw(gl: JMKOGL10; filter: jint);
begin
  //Bind the texture according to the set texture filter
  gl.glBindTexture(JMKOGL10.GL_TEXTURE_2D, textures[filter]);

  //Enable the vertex, texture and normal state
  gl.glEnableClientState(JMKOGL10.GL_VERTEX_ARRAY);
  gl.glEnableClientState(JMKOGL10.GL_TEXTURE_COORD_ARRAY);
  gl.glEnableClientState(JMKOGL10.GL_NORMAL_ARRAY);

  //Set the face rotation
  gl.glFrontFace(JMKOGL10.GL_CCW);

  //Point to our buffers
  gl.glVertexPointer(3, JMKOGL10.GL_FLOAT, 0, vertexBuffer);
  gl.glTexCoordPointer(2, JMKOGL10.GL_FLOAT, 0, textureBuffer);
  gl.glNormalPointer(JMKOGL10.GL_FLOAT, 0, normalBuffer);

  //Draw the vertices as triangles, based on the Index Buffer information
  gl.glDrawElements(JMKOGL10.GL_TRIANGLES, indicesLength + 1, JMKOGL10.GL_UNSIGNED_BYTE, indexBuffer);

  //Disable the client state before leaving
  gl.glDisableClientState(JMKOGL10.GL_VERTEX_ARRAY);
  gl.glDisableClientState(JMKOGL10.GL_TEXTURE_COORD_ARRAY);
  gl.glDisableClientState(JMKOGL10.GL_NORMAL_ARRAY);
end;

procedure TCube.loadGLTexture(gl: JMKOGL10; Context: ACContext);
var
  IStream: JIInputStream;
  bitmap:  AGBitmap;
begin
  //Get the texture from the Android resource directory
  //InputStream is
  IStream := context.getResources().openRawResource(R.drawable.crate);
  bitmap := nil;
   try
	  //BitmapFactory is an Android graphics utility for images
	  bitmap := AGBitmapFactory.decodeStream(IStream);

  except on e: JIIOException  do begin
        //Always clear and close
        IStream.close();
	IStream	:= nil;
       end;
  end;

  //Generate there texture pointer
  gl.glGenTextures(3, textures, 0);

  //Create Nearest Filtered Texture and bind it to texture 0
  gl.glBindTexture(JMKOGL10.GL_TEXTURE_2D, textures[0]);
  gl.glTexParameterf(JMKOGL10.GL_TEXTURE_2D, JMKOGL10.GL_TEXTURE_MAG_FILTER, JMKOGL10.GL_NEAREST);
  gl.glTexParameterf(JMKOGL10.GL_TEXTURE_2D, JMKOGL10.GL_TEXTURE_MIN_FILTER, JMKOGL10.GL_NEAREST);
  AOGLUtils.texImage2D(JMKOGL10.GL_TEXTURE_2D, 0, bitmap, 0);

  //Create Linear Filtered Texture and bind it to texture 1
  gl.glBindTexture(JMKOGL10.GL_TEXTURE_2D, textures[1]);
  gl.glTexParameterf(JMKOGL10.GL_TEXTURE_2D, JMKOGL10.GL_TEXTURE_MAG_FILTER, JMKOGL10.GL_LINEAR);
  gl.glTexParameterf(JMKOGL10.GL_TEXTURE_2D, JMKOGL10.GL_TEXTURE_MIN_FILTER, JMKOGL10.GL_LINEAR);
  AOGLUtils.texImage2D(JMKOGL10.GL_TEXTURE_2D, 0, bitmap, 0);

  //Create mipmapped textures and bind it to texture 2
  gl.glBindTexture(JMKOGL10.GL_TEXTURE_2D, textures[2]);
  gl.glTexParameterf(JMKOGL10.GL_TEXTURE_2D, JMKOGL10.GL_TEXTURE_MAG_FILTER, JMKOGL10.GL_LINEAR);
  gl.glTexParameterf(JMKOGL10.GL_TEXTURE_2D, JMKOGL10.GL_TEXTURE_MIN_FILTER, JMKOGL10.GL_LINEAR_MIPMAP_NEAREST);

  { * This is a change to the original tutorial, as buildMipMap does not exist anymore
   * in the Android SDK.
   *
   * We check if the GL context is version 1.1 and generate MipMaps by flag.
   * Otherwise we call our own buildMipMap implementation
   */}
 // if(gl instanceof GL11) then begin
 { if (gl instanceof GL11) then begin
	  gl.glTexParameterf(JMKOGL10.GL_TEXTURE_2D, JMKOGL10.GL_LINEAR_MIPMAP_LINEAR {GL_GENERATE_MIPMAP}, JMKOGL10.GL_TRUE);
	  AOGLUtils.texImage2D(JMKOGL10.GL_TEXTURE_2D, 0, bitmap, 0);
  end else
	  buildMipmap(gl, bitmap); }

  //Clean up
  bitmap.recycle();
end;

procedure TCube.buildMipmap(gl: JMKOGL10; bitmap: AGBitmap);
var
  level: jint;
  height, width: jint;
  bitmap2: AGBitmap;
begin
  //
  level := 0;
  //
  height := bitmap.getHeight();
  width  := bitmap.getWidth();

  //
  while ((height >= 1)  or  (width >= 1)) do begin    // ||
      //First of all, generate the texture from our bitmap and set it to the according level
      AOGLUtils.texImage2D(JMKOGL10.GL_TEXTURE_2D, level, bitmap, 0);

      //
      if ((height = 1) or (width = 1)) then exit;

      //Increase the mipmap level
      Inc(level);

      //
      height := height div 2;
      width :=  width + 2;
      bitmap2 := AGBitmap.createScaledBitmap(bitmap, width, height, true);

      //Clean up
      bitmap.recycle();
      bitmap := bitmap2;
   end;
end;

end.

