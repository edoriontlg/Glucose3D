//Used only for showing the image. You cannot set individual pixels with scribble :/
import fr.istic.si2.scribble._

//We draw the image using this
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import java.awt.Graphics2D
import java.awt.Color


//A cool github maths library, because writing my own was too painful.
//I had to import it in a sketchy way tho (see packages)
import com.github.jpbetz.subspace.Vector3
import com.github.jpbetz.subspace.Vector2
import com.github.jpbetz.subspace.Matrix4x4
import com.github.jpbetz.subspace.Vector4
import com.github.jpbetz.subspace.Quaternion


object main extends App {
  
  //
  // PARAMETRES
  // Le nombre de pas max (plus de pas = plus de détails mais plus de temps)
  val MarchCount: Float = 200
  //Le modele 3D que vous voulez rendre (Sphere, Torus, Capsule)
  val model: RenderType = InfiniteSpheres
  //La résolution
  val resolution: (Int, Int) = (512, 512)
  //Le champ de vue (field of view ou FoV)
  val FoV: Float = 90
  
  
  
  
  
  
  
  
  
  
  
  
  //Main function. Here we run path tracing function for each pixel.
  def drawer(X: Int, Y: Int, FieldOfView: Float) : Image = {
    
    //The image we will return
    var result: BufferedImage = new BufferedImage(X, Y, BufferedImage.TYPE_INT_RGB)

    
    //This prepare everything so that we can get the ray direction easily    
    val aspectRatio: Float = X.toFloat/Y.toFloat
    val cameraPos: Vector3 = new Vector3(90, -90, 90)
    
    
    
    //For each pixel we run the recursive function.
    for(x <- 0 to X-1) {
      for (y <- 0 to Y-1) {
        
        //I am not an expert in maths, even tho I like it.
        //For the calculations I got help from https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays
        
        val pX: Float = (2f * ((x + 0.5f) / X) -1f) * Math.tan(FieldOfView/2*Math.PI/180).toFloat * aspectRatio
        val pY: Float = (1f - 2f * ((y + 0.5f) / Y)) * Math.tan(FieldOfView/2*Math.PI/180).toFloat
        
        val rayDirection: Vector3 = (new Vector3(pX.toFloat, pY.toFloat, -1)).normalize.rotate(Quaternion.forEuler(new Vector3(Math.PI.toFloat/4f, 0, Math.PI.toFloat/4f)))
                                                                                         
        
        
        
        
        //Launch the recursivity (YAY FINALLY)
        val pixelData: (Int, Float) = rayMarching(cameraPos, rayDirection.normalize, 0, 0)
        
        
        
        
        //Print pixel info
        /*if (pixelData._1 != MarchCount)*/ println("Calculating: X:" + x + ", Y:" + y + "; direction : " + rayDirection)
        
        
        
        
        
        //COLORING BASED ON STEPS
        def setRGBOnSteps(data: (Int, Float) ): Unit = {
          result.setRGB(x, y, new Color(clamp(0, 255, ((data._1)/MarchCount * 255).toInt),
            clamp(0, 255, ((data._1)/MarchCount * 255).toInt),
            clamp(0, 255, ((data._1)/MarchCount * 255).toInt))
          .getRGB())
        }
         
        //COLORING BASED ON AVERAGE DISTANCE PER STEP
        def setRGBOnAverage(data: (Int, Float) ): Unit = {
          val mAverageDistancePerStep: Float = data._2/data._1.toFloat
          
          result.setRGB(x, y, new Color(clamp(0, 255, (mAverageDistancePerStep/100*255).toInt),
            clamp(0, 255, (mAverageDistancePerStep/100*255).toInt),
            clamp(0, 255, (mAverageDistancePerStep/100*255).toInt))
          .getRGB())
        }
        
        
        //Add the pixel COLORING BASED ON STEPS
        var trashcan = model match {
          case Capsule => setRGBOnSteps(pixelData)
          case Torus => setRGBOnSteps(pixelData)
          case MANDEL => setRGBOnAverage(pixelData)
          case TWIANGLE => setRGBOnAverage(pixelData) 
          case InfiniteSpheres => setRGBOnSteps(pixelData)    
          case _ => setRGBOnSteps(pixelData)
        }
        
      }
    }
    
    
    //Writes the file
    val file: File = new File("result.png")
    ImageIO.write(result, "png", file);
    
    //Open it with scribble (quickest way to display it to the user
    FromFile("result.png")
  }
  
  
  //Our main recursive function. stops when it hits something, or if it's too far away
  def rayMarching(origin: Vector3, direction: Vector3, count: Int, distance: Float): (Int, Float) = {
    if (count >= MarchCount.toInt) (MarchCount.toInt, distance)
    else {
      val md = model match {
        case Capsule => Capsule(origin)
        case Torus => Torus(origin)
        case MANDEL => MANDEL(origin)
        case TWIANGLE => TWIANGLE(origin)
        case InfiniteSpheres => ISphere(origin)
        case _ => Sphere(origin)
      }
      md < 0.00000001 || distance > 1000000000 match {
        case true => {
          (count, distance + md)
        }
        case false => rayMarching(origin + (direction.normalize * md.toFloat), direction, count + 1, distance+md)
      }
    }
  }
  
  
  //
  // MATHS HELPERS
  //
  
  def clamp(min: Float, max: Float, a: Float): Float = if (min>max-1) 0
                                               else if (a<min) min
                                               else if (a>max) max
                                               else a
                                               
  def clamp(min: Int, max: Int, a: Int): Int = if (min>max-1) 0
                                               else if (a<min) min
                                               else if (a>max) max
                                               else a
                                               
  def minF(a: Float, b: Float) : Float = a<b match {
                                                 case true => a
                                                 case false => b
                                               }
  
  def maxF(a: Float, b: Float) : Float = a>b match {
                                                 case true => a
                                                 case false => b
                                               }
  
  def length(v: Vector3) : Float = v.distanceTo(new Vector3(0, 0, 0))
  def length(v: Vector2) : Float = v.distanceTo(new Vector2(0, 0))
 
  
  
  //
  // DISTANCE ESTIMATORS
  //
                                               
  // THE MAGIC BEGINS
  //This is were I'll draw the shapes. With a bit of maths, you can do pretty cool stuff
    
  
  //
  //SPHERE
  //
  var Radius: Float = 1
  var spherePos: Vector3 = Vector3.fill(0)
  
  //Get the max distance from a sphere
  def Sphere(p: Vector3): Float = (p-spherePos).magnitude-Radius
  
  //
  //INFINITE SPHERE
  //
  var IRadius: Float = 10
  var IspherePos: Vector3 = Vector3.fill(0)
  
  //Get the max distance from a sphere
  def ISphere(p: Vector3): Float = (new Vector3(p.x%30.0f, p.y%30.0f, p.z)-IspherePos).magnitude-IRadius
  
  //
  //TORUS
  //
  val tV: Vector2 = new Vector2(1, .5f)
  def Torus(p: Vector3): Float = length(new Vector2(length(p.xz)-tV.x, p.y))-tV.y
  
  //
  //CAPSULE
  //
  val a: Vector3 = new Vector3(0,-2,0)
  val b: Vector3 = new Vector3(0,1,0)
  val capRadius: Float = 0.5f
  def Capsule(p: Vector3): Float = {
    var pa: Vector3 = p - a
    var ba: Vector3 = b - a
    var h: Float = clamp(0, 1, pa.dotProduct(ba)/ba.dotProduct(ba))
    
    length(pa-ba*h)-capRadius
  }
  
  
  
  //
  //TRIANGLE NOT WORKING
  //
  var Size: Float = 1
  
  def TWIANGLE(p: Vector3): Float = {
    var z: Vector3 = p
    var iterations: Int = 1;
    var a1: Vector3 = new Vector3(1,1,1)
    var a2: Vector3 = new Vector3(-1,-1,1)
    var a3: Vector3 = new Vector3(1,-1,-1)
    var a4: Vector3 = new Vector3(-1,1,-1)
    var c: Vector3 = null;
    var n: Int = 0
    
    var dist: Float = 0;
    var d: Float = 0;
    while (n < iterations) {
      c = a1;
      dist = length(z - a1)
      d = length(z- a2)
      if (d < dist) {
        c = a2; 
        dist = d;
      }
      d = length(z- a3)
      if (d < dist) {
        c = a3; 
        dist = d;
      }
      d = length(z-a4)
      if (d < dist) {
        c = a4; 
        dist = d;
      }
      
      z = z* Size.toFloat - c*(Size-1.0).toFloat
      n = n+1
    }
    
    //println(z.getLength())
    length(z) * Math.pow(Size, -n).toFloat
  }
  
  
  
  //
  //Mandelbub
  //
  def MANDEL(p: Vector3): Float = {
    var z: Vector3 = p
    //println(length(z))
    var dr: Float = 1
    var r: Float = 0
    var Power: Float = 1
    var iterations: Int = 1
    for (i: Int <- 0 to iterations) {
       r = length(z)

       //Convert to polar coordinate (idk what it means but it sounds cool)
       var theta: Float = Math.acos(z.z/r).toFloat
       var phi: Float = Math.atan2(z.y, z.x).toFloat
       dr = Math.pow(r, Power-1.0).toFloat*Power*dr + 1.0f
       
       var zr: Float = Math.pow(r, Power).toFloat
       theta = theta*Power
       phi = phi*Power
       
       //Convert back to cartesian coordinates (same)
       z = new Vector3((Math.sin(theta)*Math.cos(phi)).toFloat, (Math.sin(phi)*Math.sin(theta)).toFloat,  Math.cos(theta).toFloat)*zr.toFloat
       z += p
    }
    
    //println(0.5*Math.log(r)*r/dr)
    0.5f*Math.log(r).toFloat*r/dr
  }
  
  
  sealed trait RenderType
  case object MANDEL extends RenderType
  case object Sphere extends RenderType
  case object Torus extends RenderType
  case object Capsule extends RenderType
  case object TWIANGLE extends RenderType
  case object InfiniteSpheres extends RenderType

  
  //We need to draw the image to the user
  draw(drawer(resolution._1, resolution._2, FoV))
}