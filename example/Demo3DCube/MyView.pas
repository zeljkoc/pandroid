unit MyView;

{$mode objfpc}{$H+}

interface

uses
  androidr15, MyElemnet;

type

   { OpenGLRenderer }

   MyGLRenderer = class(AOGLSurfaceView.InnerRenderer)
       constructor Create(context: ACContext); virtual;

       procedure onSurfaceCreated(gl: JMKOGL10; config: JMKEEGLConfig); overload;
       procedure onSurfaceChanged(gl: JMKOGL10; width: jint; height: jint); overload;
       procedure onDrawFrame(gl: JMKOGL10); overload;
     private
       mPyramid: Pyramid;
       mCube: Cube;

      // Angle For The Pyramid
	rtri: jfloat;
      // Angle For The Cube */
	rquad: jfloat;
   end;

   { MyGLSurfaceView }

   MyGLSurfaceView = class(AOGLSurfaceView)
     renderer: MyGLRenderer;
   public
     constructor create(context: ACContext); overload;
   end;


implementation

{ MyGLSurfaceView }

constructor MyGLSurfaceView.create(context: ACContext);
begin
  inherited Create(context);
  renderer := MyGLRenderer.Create(context);
  setRenderer(renderer);
  // Request focus, otherwise key/button won't react
  requestFocus();
  setFocusableInTouchMode(true);
end;

{ MyGLSurfaceView.MyGLRenderer }

procedure MyGLRenderer.onSurfaceCreated(gl: JMKOGL10; config: JMKEEGLConfig);
begin
  gl.glShadeModel(JMKOGL10.GL_SMOOTH); 			//Enable Smooth Shading
  gl.glClearColor(0.0, 0.0, 0.0, 0.5); 	//Black Background
  gl.glClearDepthf(1.0); 					//Depth Buffer Setup
  gl.glEnable(JMKOGL10.GL_DEPTH_TEST); 			//Enables Depth Testing
  gl.glDepthFunc(JMKOGL10.GL_LEQUAL); 			//The Type Of Depth Testing To Do

  //Really Nice Perspective Calculations
  gl.glHint(JMKOGL10.GL_PERSPECTIVE_CORRECTION_HINT, JMKOGL10.GL_NICEST);
end;

procedure MyGLRenderer.onSurfaceChanged(gl: JMKOGL10;  width: jint; height: jint);
var
  aspect: jfloat;
begin
  if (height = 0) then						//Prevent A Divide By Zero By
  	height := 1; 						//Making Height Equal One


  gl.glViewport(0, 0, width, height); 	//Reset The Current Viewport
  gl.glMatrixMode(JMKOGL10.GL_PROJECTION); 	//Select The Projection Matrix
  gl.glLoadIdentity(); 					//Reset The Projection Matrix

  //Calculate The Aspect Ratio Of The Window
  AOGLU.gluPerspective(gl, 45.0, jfloat(width) / jfloat(height), 0.1, 100.0);

  gl.glMatrixMode(JMKOGL10.GL_MODELVIEW); 	//Select The Modelview Matrix
  gl.glLoadIdentity(); 					//Reset The Modelview Matrix
end;

procedure MyGLRenderer.onDrawFrame(gl: JMKOGL10);

begin
  //Clear Screen And Depth Buffer
  gl.glClear(JMKOGL10.GL_COLOR_BUFFER_BIT);
  gl.glClear(JMKOGL10.GL_DEPTH_BUFFER_BIT);

  gl.glLoadIdentity();			//Reset The Current Modelview Matrix

  //Drawing
  gl.glTranslatef(0.0, -1.2, -7.0);	//Move down 1.0 Unit And Into The Screen 7.0
  //Minor change: Scale the Cube to 80 percent, otherwise it would be too large for the Emulator screen
  gl.glScalef(0.8, 0.8, 0.8);
  gl.glRotatef(rquad, 1.0, 1.0, 1.0);	//Rotate The Square On The X axis
  mCube.draw(gl);							//Draw the Cube

  //Reset The Current Modelview Matrix
  gl.glLoadIdentity();

  gl.glTranslatef(0.0, 1.3, -6.0);	//Move up 1.3 Units and -6.0 as the origin matrix is loaded before
  gl.glRotatef(rtri, 1.0, 1.0, 0.0);	//Rotate The Triangle On The Y axis
  mPyramid.draw(gl);						//Draw the Pyramid

  //Rotation
  rtri := rtri + 1.2; 			//Increase The Rotation Variable For The Pyramid
  rquad := rquad - 1.15; 		//Decrease The Rotation Variable For The Cube
end;

constructor MyGLRenderer.Create(context: ACContext);
begin
  inherited Create;
   // Set up the buffers for these shapes
   mPyramid := Pyramid.Create;
   mCube := Cube.Create;


end;


end.

