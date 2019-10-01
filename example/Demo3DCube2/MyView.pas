unit MyView;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demo3dcube2}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, MyElement;

type

   { MyGLRenderer }

   MyGLRenderer = class(AOGLSurfaceView.InnerRenderer)
     private
	   Cube:  TCube; // Cube instance

           fogColorBuffer: JNFloatBuffer ;	//The Fog Color Buffer  ( NEW )
           // Rotation values
	   xrot: jfloat;			//X Rotation
	   yrot: jfloat;			//Y Rotation

     	  // Rotation speed values */
	  xspeed: jfloat;			//X Rotation Speed
	  yspeed: jfloat;			//Y Rotation Speed

       const
           fogFilter: jint = 0;			//Which Fog To Use ( NEW )
           z: jfloat = -5.0;			//Depth Into The Screen
           filter: jint = 0;			//Which texture filter?
           // Is light enabled */
	   light: boolean = false;
           {
	     * Init the three fog filters we will use
	     * and the fog color ( NEW )
	    }
	    fogMode: array[0..2] of jint = (
					JMKOGL10.GL_EXP,
					JMKOGL10.GL_EXP2,
					JMKOGL10.GL_LINEAR
					);
            fogColorLength = 3;
	    fogColor:array[0..fogColorLength] of jfloat = (0.1, 0.1, 0.1, 1.0);
	    {
	     * The initial light values for ambient and diffuse
	     * as well as the light position
	     }
            lightAmbientLength = 3;
	    lightAmbient: array[0..lightAmbientLength] of jfloat = (0.5, 0.5, 0.5, 1.0);
            lightDiffuseLength = 3;
	    lightDiffuse: array[0..lightDiffuseLength] of jfloat = (1.0, 1.0, 1.0, 1.0);
            lightPositionLength = 3;
	    lightPosition: array[0..lightPositionLength] of jfloat = (0.0, 0.0, 2.0, 1.0);
     private
	    // The buffers for our light values */
	    lightAmbientBuffer: JNFloatBuffer;
	    lightDiffuseBuffer: JNFloatBuffer;
	    lightPositionBuffer: JNFloatBuffer;

            // The Activity Context */
	    fContext: ACContext;
     public
       constructor Create(context: ACContext); virtual;

       procedure onSurfaceCreated(gl: JMKOGL10; config: JMKEEGLConfig); overload;
       procedure onSurfaceChanged(gl: JMKOGL10; width: jint; height: jint); overload;
       procedure onDrawFrame(gl: JMKOGL10); overload;
   end;

   { MyGLSurfaceView }

   MyGLSurfaceView = class(AOGLSurfaceView)
     renderer: MyGLRenderer;
   private
        	    {/*
	     * These variables store the previous X and Y
	     * values as well as a fix touch scale factor.
	     * These are necessary for the rotation transformation
	     * added to this lesson, based on the screen touches.
	     */}
	    oldX: jfloat;
            oldY: jfloat;

      const
        TOUCH_SCALE: jfloat = 0.2;		//Proved to be good for normal rotation
   public
     constructor create(aContext: ACContext); overload;
     function onKeyUp(keyCode: jint; event: AVKeyEvent): jboolean; overload; override;
     function onTouchEvent(event: AVMotionEvent): jboolean; overload; override;
   end;


implementation

{ MyGLSurfaceView }

constructor MyGLSurfaceView.create(aContext: ACContext);
begin
  inherited Create(aContext);
  renderer := MyGLRenderer.Create(aContext);
  setRenderer(renderer);
  // Request focus, otherwise key/button won't react
  requestFocus();
  setFocusableInTouchMode(true);
end;

function MyGLSurfaceView.onKeyUp(keyCode: jint; event: AVKeyEvent): jboolean;
begin
 {   if(keyCode = AVKeyEvent.KEYCODE_1) then    renderer.yspeed  := renderer.yspeed - 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_2)     then    renderer.yspeed := renderer.yspeed + 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_3)     then    renderer.xspeed := renderer.xspeed - 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_4)     then    renderer.xspeed := renderer.xspeed + 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_5)     then begin
	    renderer.filter := renderer.filter + 1;
	    if (renderer.filter > 2) then renderer.filter := 0;
    end;  }

    if (keyCode = AVKeyEvent.KEYCODE_DPAD_LEFT) then    renderer.yspeed  := renderer.yspeed - 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_DPAD_RIGHT)     then    renderer.yspeed := renderer.yspeed + 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_DPAD_UP)     then    renderer.xspeed := renderer.xspeed - 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_DPAD_DOWN)     then    renderer.xspeed := renderer.xspeed + 0.1
    else if (keyCode = AVKeyEvent.KEYCODE_DPAD_CENTER)     then begin
	    renderer.filter := renderer.filter + 1;
	    if (renderer.filter > 2) then renderer.filter := 0;
    end;

    //We handled the event
    Result:=inherited onKeyUp(keyCode, event);
end;

function MyGLSurfaceView.onTouchEvent(event: AVMotionEvent): jboolean;
var
  x1, y1: jfloat;
  dx, dy: jfloat;
  upperArea: jint;
  lowerArea: jint;
begin
  Result:=inherited onTouchEvent(event);
    //
  x1 := event.getX();
  y1 := event.getY();

  //If a touch is moved on the screen
  if(event.getAction() = AVMotionEvent.ACTION_MOVE) then begin
      //Calculate the change
      dx := x1 - oldX;
      dy := y1 - oldY;
      //Define an upper area of 10% on the screen
      upperArea := getHeight div 10;

      //Zoom in/out if the touch move has been made in the upper
      if (y1 < upperArea) then
              renderer.z := renderer.z - (dx * TOUCH_SCALE / (getWidth() / 16))

      //Rotate around the axis otherwise
      else begin
    	   renderer.xrot := renderer.xrot + (dy * TOUCH_SCALE);
    	   renderer.yrot := renderer.yrot + (dx * TOUCH_SCALE);
      end;
  end else

    //A press on the screen
  if (event.getAction = AVMotionEvent.ACTION_UP) then begin
        //Define an upper area of 10% to define a lower area
        upperArea := getHeight() div 10;
        lowerArea := getHeight() - upperArea;

        //
        if (y1 > lowerArea) then begin
                //Change the blend setting if the lower area left has been pressed
                if (x1 < (getWidth() / 2)) then begin
		      renderer.fogFilter := renderer.fogFilter + 1; 	//Increase fogFilter By One ( NEW )

		      //Is fogFilter Greater Than 2? ( NEW )
		      if(renderer.fogFilter > 2) then renderer.fogFilter := 0; 	//If So, Set fogFilter To Zero back again ( NEW )

                //Change the light setting if the lower area right has been pressed
                end else
        	    if (renderer.light) then renderer.light := false
            	    else renderer.light := true;
       end;
   end;

    //Remember the values
    oldX := x1;
    oldY := y1;

    //We handled the event
   Result:= true;
end;

{ MyGLSurfaceView.MyGLRenderer }

procedure MyGLRenderer.onSurfaceCreated(gl: JMKOGL10; config: JMKEEGLConfig);
begin
  //And there'll be light!
  gl.glLightfv(JMKOGL10.GL_LIGHT0, JMKOGL10.GL_AMBIENT, lightAmbientBuffer);	//Setup The Ambient Light
  gl.glLightfv(JMKOGL10.GL_LIGHT0, JMKOGL10.GL_DIFFUSE, lightDiffuseBuffer);	//Setup The Diffuse Light
  gl.glLightfv(JMKOGL10.GL_LIGHT0, JMKOGL10.GL_POSITION, lightPositionBuffer);	//Position The Light
  gl.glEnable(JMKOGL10.GL_LIGHT0);						//Enable Light 0

  //Settings
  gl.glDisable(JMKOGL10.GL_DITHER);				//Disable dithering
  gl.glEnable(JMKOGL10.GL_TEXTURE_2D);				//Enable Texture Mapping
  gl.glShadeModel(JMKOGL10.GL_SMOOTH); 				//Enable Smooth Shading
  gl.glClearColor(0.5, 0.5, 0.5, 1.0); 			        //We'll Clear To The Color Of The Fog ( Modified )
  gl.glClearDepthf(1.0); 					//Depth Buffer Setup
  gl.glEnable(JMKOGL10.GL_DEPTH_TEST); 				//Enables Depth Testing
  gl.glDepthFunc(JMKOGL10.GL_LEQUAL); 				//The Type Of Depth Testing To Do

  //The Fog/The Mist
  gl.glFogf(JMKOGL10.GL_FOG_MODE, fogMode[fogFilter]);	        //Fog Mode ( NEW )
  gl.glFogfv(JMKOGL10.GL_FOG_COLOR, fogColorBuffer);		//Set Fog Color ( NEW )
  gl.glFogf(JMKOGL10.GL_FOG_DENSITY, 0.35);			//How Dense Will The Fog Be ( NEW )
  gl.glHint(JMKOGL10.GL_FOG_HINT, JMKOGL10.GL_DONT_CARE);	//Fog Hint Value ( NEW )
  gl.glFogf(JMKOGL10.GL_FOG_START, 1.0);			//Fog Start Depth ( NEW )
  gl.glFogf(JMKOGL10.GL_FOG_END, 5.0);				//Fog End Depth ( NEW )
  gl.glEnable(JMKOGL10.GL_FOG);					//Enables GL_FOG ( NEW )

  //Really Nice Perspective Calculations
  gl.glHint(JMKOGL10.GL_PERSPECTIVE_CORRECTION_HINT, JMKOGL10.GL_NICEST);

  //Load the texture for the cube once during Surface creation
  cube.loadGLTexture(gl, fContext);
end;

procedure MyGLRenderer.onSurfaceChanged(gl: JMKOGL10;  width: jint; height: jint);

begin
  if (height = 0) then 				//Prevent A Divide By Zero By
  	height := 1; 				//Making Height Equal One


  gl.glViewport(0, 0, width, height); 	         //Reset The Current Viewport
  gl.glMatrixMode(JMKOGL10.GL_PROJECTION); 	//Select The Projection Matrix
  gl.glLoadIdentity(); 				//Reset The Projection Matrix

  //Calculate The Aspect Ratio Of The Window
  AOGLU.gluPerspective(gl, 45.0, jfloat(width) / jfloat(height), 0.1, 100.0);

  gl.glMatrixMode(JMKOGL10.GL_MODELVIEW); 	//Select The Modelview Matrix
  gl.glLoadIdentity(); 				//Reset The Modelview Matrix
end;

procedure MyGLRenderer.onDrawFrame(gl: JMKOGL10);

begin
  //Clear Screen And Depth Buffer
  gl.glClear(JMKOGL10.GL_DEPTH_BUFFER_BIT);
  gl.glClear(JMKOGL10.GL_COLOR_BUFFER_BIT);

  gl.glLoadIdentity();				//Reset The Current Modelview Matrix

  //Check if the light flag has been set to enable/disable lighting
  if(light) then gl.glEnable(JMKOGL10.GL_LIGHTING)
   else          gl.glDisable(JMKOGL10.GL_LIGHTING);

  //Set Fog Mode ( NEW )
  gl.glFogf(JMKOGL10.GL_FOG_MODE, fogMode[fogFilter]);

  //Drawing
  gl.glTranslatef(0.0, 0.0, z);		//Move z units into the screen
  gl.glScalef(0.8, 0.8, 0.8); 		//Scale the Cube to 80 percent, otherwise it would be too large for the screen

  //Rotate around the axis based on the rotation matrix (rotation, x, y, z)
  gl.glRotatef(xrot, 1.0, 0.0, 0.0);	//X
  gl.glRotatef(yrot, 0.0, 1.0, 0.0);	//Y

  cube.draw(gl, filter);		//Draw the Cube

  //Change rotation factors
  xrot := xrot + xspeed;
  yrot := yrot + yspeed;
end;



constructor MyGLRenderer.Create(context: ACContext);
var
   byteBuf: JNByteBuffer;

begin
  inherited Create;

  //
  fContext := context;

{zc  //Set this as Renderer
  setRenderer(fContext);
  //Request focus, otherwise buttons won't react
  requestFocus();
  self.setFocusableInTouchMode(true);  }


  //
  byteBuf := JNByteBuffer.allocateDirect((lightAmbientLength + 1)* 4);
  byteBuf.order(JNByteOrder.nativeOrder());
  lightAmbientBuffer := byteBuf.asFloatBuffer();
  lightAmbientBuffer.put(lightAmbient);
  lightAmbientBuffer.position(0);

  byteBuf := JNByteBuffer.allocateDirect((lightDiffuseLength + 1) * 4);
  byteBuf.order(JNByteOrder.nativeOrder());
  lightDiffuseBuffer := byteBuf.asFloatBuffer();
  lightDiffuseBuffer.put(lightDiffuse);
  lightDiffuseBuffer.position(0);

  byteBuf := JNByteBuffer.allocateDirect((lightPositionLength + 1) * 4);
  byteBuf.order(JNByteOrder.nativeOrder());
  lightPositionBuffer := byteBuf.asFloatBuffer();
  lightPositionBuffer.put(lightPosition);
  lightPositionBuffer.position(0);

  //Build the new Buffer ( NEW )
  byteBuf := JNByteBuffer.allocateDirect((fogColorLength + 1) * 4);
  byteBuf.order(JNByteOrder.nativeOrder());
  fogColorBuffer := byteBuf.asFloatBuffer();
  fogColorBuffer.put(fogColor);
  fogColorBuffer.position(0);

  //
  cube := TCube.Create;

end;


end.


