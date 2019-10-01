unit MyElemnet;

{$mode objfpc}{$H+}

interface

uses
  androidr15;

type
   { Pyramid }

   Pyramid = class
   private
      vertexBuffer: JNFloatBuffer;  // The buffer holding the vertices */
      colorBuffer: JNFloatBuffer;   // The buffer holding the color values */
    const
       verticesLength = 35;
       vertices: array[0..verticesLength] of jfloat = (
					         0.0,  1.0, 0.0,		//Top Of Triangle (Front)
						-1.0, -1.0, 1.0,		//Left Of Triangle (Front)
						 1.0, -1.0, 1.0,		//Right Of Triangle (Front)
						 0.0,  1.0, 0.0,		//Top Of Triangle (Right)
						 1.0, -1.0, 1.0,		//Left Of Triangle (Right)
						 1.0, -1.0, -1.0,	//Right Of Triangle (Right)
						 0.0,  1.0, 0.0,		//Top Of Triangle (Back)
						 1.0, -1.0, -1.0,	//Left Of Triangle (Back)
						-1.0, -1.0, -1.0,	//Right Of Triangle (Back)
						 0.0,  1.0, 0.0,		//Top Of Triangle (Left)
						-1.0, -1.0, -1.0,	//Left Of Triangle (Left)
						-1.0, -1.0, 1.0		//Right Of Triangle (Left)
											);
       colorsLength = 47;
       colors: array[0..colorsLength] of jfloat = (
			    		1.0, 0.0, 0.0, 1.0, //Red
			    		0.0, 1.0, 0.0, 1.0, //Green
			    		0.0, 0.0, 1.0, 1.0, //Blue
			    		1.0, 0.0, 0.0, 1.0, //Red
			    		0.0, 0.0, 1.0, 1.0, //Blue
			    		0.0, 1.0, 0.0, 1.0, //Green
			    		1.0, 0.0, 0.0, 1.0, //Red
			    		0.0, 1.0, 0.0, 1.0, //Green
			    		0.0, 0.0, 1.0, 1.0, //Blue
			    		1.0, 0.0, 0.0, 1.0, //Red
			    		0.0, 0.0, 1.0, 1.0, //Blue
			    		0.0, 1.0, 0.0, 1.0 	//Green
						    					);
   public
     constructor Create; virtual;
     procedure Draw(gl: JMKOGL10);
   end;

   { Cube }

   Cube = class
   private
       vertexBuffer: JNFloatBuffer;  // The buffer holding the vertices
       colorBuffer: JNFloatBuffer;  // The buffer holding the color values */
       indexBuffer: JNByteBuffer ;  // The buffer holding the indices */

      const
         verticesLength = 23;
         vertices: array[0..verticesLength] of jfloat = (  // Vertices of the 6 faces
			            -1.0, -1.0, -1.0,	//lower back left (0)
			             1.0, -1.0, -1.0,		//lower back right (1)
			             1.0,  1.0, -1.0,		//upper back right (2)
			            -1.0,  1.0, -1.0,		//upper back left (3)
			            -1.0, -1.0,  1.0,	//lower front left (4)
			             1.0, -1.0,  1.0,		//lower front right (5)
			             1.0,  1.0,  1.0,		//upper front right (6)
			            -1.0,  1.0,  1.0		//upper front left (7)
			   	);
         colorsLength = 31;
         colors: array[0..colorsLength] of jfloat = (  // Colors of the 6 faces
                          0.0,  1.0,  0.0,  1.0,
			  0.0,  1.0,  0.0,  1.0,
			  1.0,  0.5,  0.0,  1.0,
			  1.0,  0.5,  0.0,  1.0,
			  1.0,  0.0,  0.0,  1.0,
			  1.0,  0.0,  0.0,  1.0,
			  0.0,  0.0,  1.0,  1.0,
			  1.0,  0.0,  1.0,  1.0
			  );
         indicesLength = 35;
         indices:array[0..indicesLength] of jbyte = (
			            0, 4, 5,    0, 5, 1,
			            1, 5, 6,    1, 6, 2,
			            2, 6, 7,    2, 7, 3,
			            3, 7, 4,    3, 4, 0,
			            4, 7, 6,    4, 6, 5,
			            3, 0, 1,    3, 1, 2
    				    );

   public
     constructor Create; virtual;
     procedure Draw(gl: JMKOGL10);
   end;


implementation


{ Pyramid }

constructor Pyramid.Create;
var
  byteBuf: JNByteBuffer;
begin
    //
    byteBuf := JNByteBuffer.allocateDirect((verticesLength  + 1) * 4);
    byteBuf.order(JNByteOrder.nativeOrder());
    vertexBuffer := byteBuf.asFloatBuffer();
    vertexBuffer.put(vertices);
    vertexBuffer.position(0);

    //
    byteBuf := JNByteBuffer.allocateDirect((colorsLength + 1) * 4);
    byteBuf.order(JNByteOrder.nativeOrder());
    colorBuffer := byteBuf.asFloatBuffer();
    colorBuffer.put(colors);
    colorBuffer.position(0);
end;

procedure Pyramid.Draw(gl: JMKOGL10);
begin
    //Set the face rotation
    gl.glFrontFace(JMKOGL10.GL_CW);

    //Point to our buffers
    gl.glVertexPointer(3, JMKOGL10.GL_FLOAT, 0, vertexBuffer);
    gl.glColorPointer(4, JMKOGL10.GL_FLOAT, 0, colorBuffer);

    //Enable the vertex and color state
    gl.glEnableClientState(JMKOGL10.GL_VERTEX_ARRAY);
    gl.glEnableClientState(JMKOGL10.GL_COLOR_ARRAY);

    //Draw the vertices as triangles
    gl.glDrawArrays(JMKOGL10.GL_TRIANGLES, 0, (verticesLength  + 1) div 3);

    //Disable the client state before leaving
    gl.glDisableClientState(JMKOGL10.GL_VERTEX_ARRAY);
    gl.glDisableClientState(JMKOGL10.GL_COLOR_ARRAY);
end;

{ Cube }

constructor Cube.Create;
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
   byteBuf := JNByteBuffer.allocateDirect((colorsLength + 1) * 4);
   byteBuf.order(JNByteOrder.nativeOrder());
   colorBuffer := byteBuf.asFloatBuffer();
   colorBuffer.put(colors);
   colorBuffer.position(0);

   //
   indexBuffer := JNByteBuffer.allocateDirect(indicesLength + 1);
   indexBuffer.put(indices);
   indexBuffer.position(0);
end;

procedure Cube.Draw(gl: JMKOGL10);
begin
   //Set the face rotation
   gl.glFrontFace(JMKOGL10.GL_CW);

   //Point to our buffers
   gl.glVertexPointer(3, JMKOGL10.GL_FLOAT, 0, vertexBuffer);
   gl.glColorPointer(4, JMKOGL10.GL_FLOAT, 0, colorBuffer);

   //Enable the vertex and color state
   gl.glEnableClientState(JMKOGL10.GL_VERTEX_ARRAY);
   gl.glEnableClientState(JMKOGL10.GL_COLOR_ARRAY);

   //Draw the vertices as triangles, based on the Index Buffer information
   gl.glDrawElements(JMKOGL10.GL_TRIANGLES, 36, JMKOGL10.GL_UNSIGNED_BYTE, indexBuffer);

   //Disable the client state before leaving
   gl.glDisableClientState(JMKOGL10.GL_VERTEX_ARRAY);
   gl.glDisableClientState(JMKOGL10.GL_COLOR_ARRAY);

end;

end.

