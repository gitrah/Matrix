package com.hartenbower.matrix
import scala.collection.JavaConverters._
import java.util.Random
import   scala.swing._                                                                
import scala.swing.event._
import GridBagPanel._

import org.apache.log4j.Logger


import java.awt.image.BufferedImage                                           
import java.io.File                                                           
import javax.imageio.ImageIO                                                  

class ImagePanel extends Panel                                                
{                                                                             
  private var _imagePath = ""                                                 
  private var _bufferedImage:BufferedImage = null                              

  def imagePath = _imagePath                                                  
  def bufferedImage = _bufferedImage                                                  

  def imagePath_=(value:String)                                               
  {                                                                           
    _imagePath = value                                                        
    _bufferedImage = ImageIO.read(new File(_imagePath))                        
  }                                                                           

  def bufferedImage_=(bi:BufferedImage)                                               
  {                                                                           
    _bufferedImage = bi                        
  }                                                                           


  override def paintComponent(g:Graphics2D) =                                 
  {                                                                           
    if (null != _bufferedImage) g.drawImage(_bufferedImage, 0, 0, null)         
  }                                                                           
}                                                                             

object ImagePanel                                                             
{                                                                             
  def apply() = new ImagePanel()                                              
} 

class MViewer(val m: MatrixD)( dims: (Int,Int) = m.dims) extends Frame {
  val log = Logger.getLogger("MViewer")
  val rnd = new Random(System.currentTimeMillis)
  val (rows,cols) = m.dims()
  val scaley= dims._2/(rows * 1d)
  val scalex= dims._1/(cols* 1d)
  val imgW = (cols * scalex).asInstanceOf[Int]
  val imgH = (rows * scaley).asInstanceOf[Int]
  val isx = math.max(scalex.asInstanceOf[Int],1)
  val isy = math.max(scaley.asInstanceOf[Int],1)
  val sample_size =isx*isy
  val sample = new Array[Int](sample_size)
  
  val bye = new Button {
    text = "Exit"
  }
  val byeSize = bye.preferredSize
  val theFont = new Font("Arial", 0, 12).deriveFont(java.awt.Font.BOLD)

  val bufferedImage :BufferedImage = new BufferedImage( imgW, imgH,	BufferedImage.TYPE_USHORT_GRAY  )
  
  def createImage() {
  	val raster = bufferedImage.getRaster()
  	var i = 0
  	var j = 0
  	var s : Short = 0
  	while(i < m.nRows) {
  	  j = 0
  	  while(j < m.nCols) {
  	    //println("i,j" + i + ", " + j)
  	    Util.ArrayUtil.fill(sample, m.elements(i * m.nCols + j).asInstanceOf[Int])
  	    raster.setSamples((j*scalex).asInstanceOf[Int],(i * scaley).asInstanceOf[Int],isx, isy, 0, sample)
  	    j+=1
  	  }
  	  i+=1
  	}
  }
  createImage
  
  val imagePanel = new ImagePanel
  imagePanel.bufferedImage=bufferedImage
  //imagePanel.size =(new Dimension(bufferedImage.getWidth(), bufferedImage.getHeight()))
  
  //println("size " + imagePanel.size)
  val top = new MainFrame {
    title = "MViewer"

    contents = new BoxPanel(Orientation.Vertical) {
        contents += imagePanel
        contents += bye
        //size = imagePanel.size
    }
    
    size = imagePanel.size
    
    listenTo( bye)
    var nClicks = 0
    reactions += {
      case ButtonClicked(btn) ⇒
        btn match {
          case b if btn == bye ⇒
            close()
        }
    }
  }
  def show = { 
    top.visible = true; 
    top.size =new java.awt.Dimension(imgW , imgH + (2.5 * byeSize.getHeight()).asInstanceOf[Int])
    
  }
}