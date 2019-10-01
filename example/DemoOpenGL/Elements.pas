{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
unit Elements;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demoopengl}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
;

type

    { Triangle }

    Triangle = class
    private
       vertexBuffer: JNFloatBuffer;
    const
        COORDS_PER_VERTEX: jint = 3;
        CoordsLength = 8;
        triangleCoords: array[0..CoordsLength] of jfloat = (
            // in counterclockwise order:
            0.0,  0.622008459, 0.0,// top
           -0.5, -0.311004243, 0.0,// bottom left
            0.5, -0.311004243, 0.0 // bottom right
            );
        color: array[0..3] of jfloat = ( 0.63671875, 0.76953125, 0.22265625, 0.0 );
    public
      constructor Create; virtual;
      procedure Draw(gl: JMKOGL10);
    end;

     { Square }

     Square = class
     private
       vertexBuffer: JNFloatBuffer;
       drawListBuffer: JNShortBuffer;
     const
           // number of coordinates per vertex in this array
         COORDS_PER_VERTEX: jint = 3;
         CoordsLength = 11;
         squareCoords: array[0..CoordsLength] of jfloat = (
            -0.5,  0.5, 0.0,   // top left
            -0.5, -0.5, 0.0,   // bottom left
             0.5, -0.5, 0.0,   // bottom right
             0.5,  0.5, 0.0 ); // top right
          OderLength = 5;
          drawOrder: array[0..OderLength] of jshort = ( 0, 1, 2, 0, 2, 3 ); // order to draw vertices
     public
       const
         color: array[0..3] of jfloat = ( 0.2, 0.709803922, 0.898039216, 1.0 );
     public
        constructor Create; virtual;
        procedure Draw(gl: JMKOGL10);
     end;

implementation

{ Square }

constructor Square.Create;
var
 bb: JNByteBuffer;
 dlb: JNByteBuffer;
begin
    // initialize vertex byte buffer for shape coordinates
     bb := JNByteBuffer.allocateDirect(
     // (# of coordinate values * 4 bytes per float)
            (CoordsLength + 1) * 4);
    bb.order(JNByteOrder.nativeOrder());
    vertexBuffer := bb.asFloatBuffer();
    vertexBuffer.put(squareCoords);
    vertexBuffer.position(0);

    // initialize byte buffer for the draw list
     dlb := JNByteBuffer.allocateDirect(
            // (# of coordinate values * 2 bytes per short)
            (OderLength + 1) * 2);
    dlb.order(JNByteOrder.nativeOrder());
    drawListBuffer := dlb.asShortBuffer();
    drawListBuffer.put(drawOrder);
    drawListBuffer.position(0);
end;

procedure Square.Draw(gl: JMKOGL10);
begin
    // Since this shape uses vertex arrays, enable them
    gl.glEnableClientState(JMKOGL10.GL_VERTEX_ARRAY);

    // draw the shape
    gl.glColor4f(       // set color
            color[0], color[1],
            color[2], color[3]);
    gl.glVertexPointer( // point to vertex data:
            COORDS_PER_VERTEX,
            JMKOGL10.GL_FLOAT, 0, vertexBuffer);
    gl.glDrawElements(  // draw shape:
            JMKOGL10.GL_TRIANGLES,
             (OderLength + 1), JMKOGL10.GL_UNSIGNED_SHORT,
            drawListBuffer);

    // Disable vertex array drawing to avoid
    // conflicts with shapes that don't use it
    gl.glDisableClientState(JMKOGL10.GL_VERTEX_ARRAY);
end;

{ Triangle }

constructor Triangle.Create;
var
   bb: JNByteBuffer;
begin
    // initialize vertex byte buffer for shape coordinates
    bb := JNByteBuffer.allocateDirect(
            // (number of coordinate values * 4 bytes per float)
            (CoordsLength + 1) * 4 );
    // use the device hardware's native byte order
    bb.order(JNByteOrder.nativeOrder());
    // create a floating point buffer from the ByteBuffer
    vertexBuffer :=  bb.asFloatBuffer();
    // add the coordinates to the FloatBuffer
    vertexBuffer.put(triangleCoords);

    // set the buffer to read the first coordinate
    vertexBuffer.position(0);
end;

procedure Triangle.Draw(gl: JMKOGL10);
begin
    // Since this shape uses vertex arrays, enable them
    gl.glEnableClientState(JMKOGL10.GL_VERTEX_ARRAY);

    // draw the shape
    gl.glColor4f(       // set color:
            color[0], color[1],
            color[2], color[3]);
    gl.glVertexPointer( // point to vertex data:
            COORDS_PER_VERTEX,
            JMKOGL10.GL_FLOAT, 0, vertexBuffer);
    gl.glDrawArrays(    // draw shape:
            JMKOGL10.GL_TRIANGLES, 0,
            (CoordsLength + 1) div COORDS_PER_VERTEX);

    // Disable vertex array drawing to avoid
    // conflicts with shapes that don't use it
    gl.glDisableClientState(JMKOGL10.GL_VERTEX_ARRAY);

end;

end.

