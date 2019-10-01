{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoOpenGL/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoOpeGL;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demoopengl}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, Elements;


type

  { MainActivity }

  MainActivity = class(AAActivity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onPause; overload; override;
    procedure onResume; overload; override;
  public
    mGLView:  AOGLSurfaceView;
  public
  end;

 { MyGLSurfaceView }

 MyGLSurfaceView = class(AOGLSurfaceView)
   constructor create(para1: ACContext); overload;
   function onTouchEvent(e: AVMotionEvent): jboolean; overload; override;
 public
   type
     { MyGLRenderer }
     MyGLRenderer = class(InnerRenderer)
       procedure onSurfaceCreated(gl: JMKOGL10; config: JMKEEGLConfig); overload;
       procedure onSurfaceChanged(gl: JMKOGL10; width: jint; height: jint); overload;
       procedure onDrawFrame(gl: JMKOGL10); overload;
       function getAngle: Real;
       procedure setAngle(angle: Real);
     private
       mTriangle: Triangle;
       mSquare: Square;
       mAngle: Real;
     end;
 private
    const
      TOUCH_SCALE_FACTOR = 180 / 320;
 private
    mPreviousX: real;
    mPreviousY: real;
 public
    mRenderer: MyGLRenderer;
 end;

implementation

{ MyGLSurfaceView.MyGLRenderer }

procedure MyGLSurfaceView.MyGLRenderer.onSurfaceCreated(gl: JMKOGL10; config: JMKEEGLConfig);
begin
  // Set the background frame color
  gl.glClearColor(0, 0, 0, 1);

  mTriangle := Triangle.Create;
  mSquare := Square.Create;
end;

procedure MyGLSurfaceView.MyGLRenderer.onSurfaceChanged(gl: JMKOGL10; width: jint; height: jint);
var
  ratio: Real;
begin
    // Adjust the viewport based on geometry changes
    // such as screen rotations
    gl.glViewport(0, 0, width, height);

    // make adjustments for screen ratio
    ratio := Real(width / height);
    gl.glMatrixMode(JMKOGL10.GL_PROJECTION);        // set matrix to projection mode
    gl.glLoadIdentity();                        // reset the matrix to its default state
    gl.glFrustumf(-ratio, ratio, -1, 1, 3, 7);  // apply the projection matrix
end;

procedure MyGLSurfaceView.MyGLRenderer.onDrawFrame(gl: JMKOGL10);
begin
    // Draw background color
    gl.glClear(JMKOGL10.GL_COLOR_BUFFER_BIT); // JMKOGL10.GL_COLOR_BUFFER_BIT | JMKOGL10.GL_DEPTH_BUFFER_BIT);

    // Set GL_MODELVIEW transformation mode
    gl.glMatrixMode(JMKOGL10.GL_MODELVIEW);
    gl.glLoadIdentity();   // reset the matrix to its default state

    // When using GL_MODELVIEW, you must set the view point
    AOGLU.gluLookAt(gl, 0, 0, -3, 0, 0, 0, 0, 1.0, 0.0);

    // Draw square
    mSquare.draw(gl);

    // Create a rotation for the triangle

    // Use the following code to generate constant rotation.
    // Leave this code out when using TouchEvents.
    // long time = SystemClock.uptimeMillis() % 4000L;
    // float angle = 0.090f * ((int) time);

    gl.glRotatef(mAngle, 0.0, 0.0, 1.0);

    // Draw triangle
    mTriangle.draw(gl);
end;

function MyGLSurfaceView.MyGLRenderer.getAngle: Real;
begin
  Result := mAngle;
end;

procedure MyGLSurfaceView.MyGLRenderer.setAngle(angle: Real);
begin
  mAngle:= angle;
end;


{ MyGLSurfaceView }

constructor MyGLSurfaceView.create(para1: ACContext);
begin
  inherited create(para1);
   // Set the Renderer for drawing on the GLSurfaceView
   mRenderer := MyGLRenderer.create;
   setRenderer(mRenderer);

  // Render the view only when there is a change in the drawing data
  setRenderMode(AOGLSurfaceView.RENDERMODE_WHEN_DIRTY);
end;

function MyGLSurfaceView.onTouchEvent(e: AVMotionEvent): jboolean;
var
 x1, y1, dx, dy: real;
begin
    // MotionEvent reports input details from the touch screen
    // and other input controls. In this case, we are only
    // interested in events where the touch position changed.

    x1 := e.getX();
    y1 := e.getY();

    case e.getAction of
      AVMotionEvent.ACTION_MOVE: begin
         dx := x1 - mPreviousX;
         dy := y1 - mPreviousY;

         // reverse direction of rotation above the mid-line
         if (y1 > getHeight() / 2) then dx := dx * -1 ;

         // reverse direction of rotation to left of the mid-line
         if (x1 < getWidth() / 2) then dy := dy * -1 ;

         mRenderer.setAngle(
                        mRenderer.getAngle() +
                        ((dx + dy) * TOUCH_SCALE_FACTOR));  // = 180.0f / 320

         requestRender();
      end;
    end;

    mPreviousX := x1;
    mPreviousY := y1;
    Result := true;
end;

//////////////////////////

procedure MainActivity.onCreate(savedInstanceState: AOBundle);

begin
  inherited onCreate(savedInstanceState);

//  requestWindowFeature(AVWindow.FEATURE_NO_TITLE);
  getWindow().setFlags(AVWindowManager.InnerLayoutParams.FLAG_FULLSCREEN,
  AVWindowManager.InnerLayoutParams.FLAG_FULLSCREEN);

  mGLView := MyGLSurfaceView.create(Self);
  setContentView(mGLView);
 // setContentView(MyGLSurfaceView.create(Self));
end;

procedure MainActivity.onPause;
begin
  inherited onPause;
 mGLView.onPause();
end;

procedure MainActivity.onResume;
begin
  inherited onResume;
 mGLView.onResume();
end;



end.
