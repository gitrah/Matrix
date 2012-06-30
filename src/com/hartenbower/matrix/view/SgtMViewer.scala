package com.hartenbower.matrix.view
import javax.swing._
import com.hartenbower.matrix.MatrixD
import java.awt.Toolkit
import java.awt.BorderLayout
import java.awt.FlowLayout
import java.awt.MediaTracker
import java.awt.event.ActionListener
import java.awt.print.PrinterJob
import java.awt.Color
import java.awt.Font
import java.awt.AWTEvent
import java.awt.Dimension
import gov.noaa.pmel.sgt.JPane
import gov.noaa.pmel.sgt.swing.JClassTree
import gov.noaa.pmel.util.Range2D
import gov.noaa.pmel.sgt.demo.TestData
import gov.noaa.pmel.sgt.Layer
import gov.noaa.pmel.util.Dimension2D
import gov.noaa.pmel.sgt.StackedLayout
import gov.noaa.pmel.sgt.Logo
import gov.noaa.pmel.sgt.CartesianGraph
import gov.noaa.pmel.sgt.LinearTransform
import gov.noaa.pmel.sgt.PlainAxis
import gov.noaa.pmel.util.Point2D
import gov.noaa.pmel.sgt.SGLabel
import gov.noaa.pmel.sgt.PointAttribute
import gov.noaa.pmel.sgt.beans.Page
import gov.noaa.pmel.sgt.beans.PanelModel
import gov.noaa.pmel.sgt.beans.DataModel
import gov.noaa.pmel.sgt.dm.SGTData
import gov.noaa.pmel.sgt.GridAttribute
import gov.noaa.pmel.sgt.LineAttribute
import gov.noaa.pmel.sgt.VectorAttribute
import gov.noaa.pmel.sgt.dm.SimplePoint

object SgtMViewer {
  
  def graph(x: MatrixD, y: MatrixD, title : String) {
    val frame = new SgtMViewer(x,y,title)
    frame.pack
    val screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    val frameSize = frame.getSize();
    if (frameSize.height > screenSize.height) {
      frameSize.height = screenSize.height;
    }
    if (frameSize.width > screenSize.width) {
      frameSize.width = screenSize.width;
    }
    frame.setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);
    /**
     * Since the current version of SGT Beans doesn't directly support
     * resizing disable resizing and make the frame visible.
     */
    frame.setResizable(false);
    frame.setVisible(true);
  }
}
/*
 *   def tree_actionPerformed(e: java.awt.event.ActionEvent) {
    val ct = new JClassTree()
    ct.setModal(false)
    ct.setJPane(mainPane_)
    ct.show()
  }

  class MyAction extends java.awt.event.ActionListener {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      val obj = event.getSource();
      if (obj == space_) {
        System.out.println("  <<Mark>>");
      }
      if (obj == tree_)
        tree_actionPerformed(event);
    }
  }

 */
class SgtMViewer(x: MatrixD, y: MatrixD, title: String) extends JFrame /*with ActionListener */{

  val page = new Page
  var panelModel = new PanelModel
  val dataModel = new DataModel
  var mainPane : JPane = null
  val graphicPanel = new JPanel(new BorderLayout());

  var printMenuItem: JMenuItem = null
  var exitMenuItem: JMenuItem = null
  var resetZoomMenuItem: JMenuItem = null
  var classTreeMenuItem: JMenuItem = null
  var filePageMenuItem: JMenuItem = null
  var panelModelMenuItem: JMenuItem = null
  val pageFormat = PrinterJob.getPrinterJob().defaultPage();

  setTitle(title)
  //setJMenuBar(makeMenuBar)
  enableEvents(AWTEvent.WINDOW_EVENT_MASK);
  getContentPane().setLayout(new BorderLayout());
  /**
   * Create the graphic and add it to the content pane of the JFrame.
   */
  //createGraphic();
  getContentPane().add(graphicPanel, BorderLayout.CENTER);
  graphicPanel.add(makeGraph(x,y))

  def createGraphic() {

    /**
     * Add the page object to graphicPanel and set dataModel.
     */
    graphicPanel.add(page, BorderLayout.CENTER);
    page.setDataModel(dataModel);
    /**
     * Create panelModel by de-serializing an existing PanelModel.  The
     * file SBExample3.xml, was created using gov.noaa.pmel.sgt.beans.PanelModelEditor.
     */
    panelModel = PanelModel.loadFromXML(getClass().getResource("SgtMViewer.xml").openStream());
    /**
     * Set panelModel.
     */
    page.setPanelModel(panelModel)
    /**
     * To add data to dataModel, we first need to find the PanelHolder and
     * DataGroup to which we want to add data.  The SBExample3.xml panelModel
     * was created with a Panel named "TimePanel" and a DataGroup named
     * "TimeSeries".  It is possible to query panelModel to determine what
     * PanelHolders and DataGroups are available.
     */
    val lAttr = new LineAttribute(LineAttribute.MARK, 51, Color.red.darker());

    /**
     *  has a Panel named "GridPanel" and a DataGroup named
     * "GridData".  We will use these with a GridAttribute object to create
     * a grid plot.
     */
    val pointPanel = panelModel.findPanelHolder("PointPanel")
    val pointData = pointPanel.findDataGroup("PointData")
    val pointLegend = pointPanel.findLegend("ColorLegend")
    
		val pattr = new PointAttribute(20, Color.red);
		val pfont = new Font("Helvetica", Font.PLAIN, 12);
		pattr.setLabelFont(pfont);
		pattr.setLabelColor(Color.blue);
		pattr.setLabelHeightP(0.1);
		//pattr.setDrawLabel(true);    
    /**
     * Add data to PanelHolder, gridPanel, and DataGroup, gridData.
     */
  //  dataModel.addData(grid, pattr, gridPanel, gridData, gridLegend);
    /**
     * Get a vector from the netCDF file with a stride of 2.
     */
    //SGTData vector = gridReader.getVectorGrid(2);
    /**
     * Create a VectorAttribute
     */
    val blackish = Color.black;
    val vAttr = new VectorAttribute(VectorAttribute.SCALED_HEAD,
                                                1.0, blackish, 0.3);
    vAttr.setWidth(1.5f);
    /**
     * Add vector to PanelHolder, gridPanel, and DataGroup, gridData
     */
    //dataModel.addData(vector, vAttr, gridPanel, gridData, null);
  }

  
  
  def makeGraph(x: MatrixD, y: MatrixD): JPane = {
    /*
         * This example creates a very simple plot from scratch (not using one of
         * the sgt.awt classes) to display a Collection of points.
         */
    /*
         * Create a Pane, place in the center of the Applet and set the layout to be
         * StackedLayout.
         */
    mainPane = new JPane("Point Plot Demo", new Dimension(553, 438));
    mainPane.setLayout(new StackedLayout());
    mainPane.setBackground(Color.white);
    /*
         * Create a Collection of points using the TestData class.
         */
    val ranges: Array[(Double, Double)] = x.featureMinMax()
    val xrange = new Range2D(ranges(0)._1, ranges(0)._2, (ranges(0)._2 - ranges(0)._1) / 25)
    val yrange = new Range2D(ranges(1)._1, ranges(1)._2, (ranges(1)._2 - ranges(1)._1) / 25)
    println("xrange "+ xrange)
    println("yrange "+ yrange)
    val pos = new gov.noaa.pmel.sgt.dm.Collection[SimplePoint]()
    val neg = new gov.noaa.pmel.sgt.dm.Collection[SimplePoint]()
    val (m,n) = x.dims()
    var i = 0
    while(i < m) {
      if(y(i+1,1) == 1d) {
      	pos.add( new SimplePoint(x(i + 1,1),x(i + 1,2)))
      }else {
        neg.add( new SimplePoint(x(i + 1,1),x(i + 1,2)))
      }
      
      i+=1
    }
    /*
         * xsize, ysize are the width and height in physical units of the Layer
         * graphics region.
         * 
         * xstart, xend are the start and end points for the X axis ystart, yend are
         * the start and end points for the Y axis
         */
    var xsize = 4.0;
    var xstart = 0.6;
    var xend = 3.5;
    var ysize = 3.0;
    var ystart = 0.6;
    var yend = 2.75;
    /*
         * Create the layer and add it to the Pane.
         */
    val posLayer = new Layer("Layer 1", new Dimension2D(xsize, ysize));
    val negLayer = new Layer("Layer 2", new Dimension2D(xsize, ysize));
    mainPane.add(posLayer);
    mainPane.add(negLayer);
    /*
         * create and add image as a Logo to the posLayer
         */
    val img = null; // this.getToolkit().getImage( getClass().getResource("ncBrowse48.gif"));
    //
    // wait for image to be loaded
    //
    //        if (img != null) {
    //            val mt = new MediaTracker(this);
    //                mt.addImage(img, 0);
    //                mt.waitForAll();
    //                if (mt.isErrorAny())
    //                    System.err.println("JPointDemo: Error loading image");
    //        }
    val logo = new Logo(new Point2D.Double(0.0, 0.0), Logo.BOTTOM, Logo.LEFT);
    logo.setId("ncBrowse logo");
    // logo.setImage(img);
    posLayer.addChild(logo);
    /*
         * Create a CartesianGraph and transforms.
         */

    val posGraph = new CartesianGraph("Pos Graph");
    val negGraph = new CartesianGraph("Neg Graph");
    posLayer.setGraph(posGraph);
    negLayer.setGraph(negGraph);
    val xt = new LinearTransform(xstart, xend, xrange.start, xrange.end);
    val yt = new LinearTransform(ystart, yend, yrange.start, yrange.end);
    posGraph.setXTransform(xt);
    posGraph.setYTransform(yt);
    negGraph.setXTransform(xt);
    negGraph.setYTransform(yt);
    /*
         * Create the bottom axis, set its range in user units and its origin. Add
         * the axis to the graph.
         */
    val xLabel = "X Label";

    val xbot = new PlainAxis("Botton Axis");
    xbot.setRangeU(xrange);
    xbot.setLocationU(new Point2D.Double(xrange.start, yrange.start));
    val xbfont = new Font("Helvetica", Font.ITALIC, 14);
    xbot.setLabelFont(xbfont);
    val xtitle = new SGLabel("xaxis title", xLabel, new Point2D.Double(0.0,
      0.0));
    val xtfont = new Font("Helvetica", Font.PLAIN, 14);
    xtitle.setFont(xtfont);
    xtitle.setHeightP(0.2);
    xbot.setTitle(xtitle);
    posGraph.addXAxis(xbot);
    /*
         * Create the left axis, set its range in user units and its origin. Add the
         * axis to the graph.
         */
    val yLabel = "Y Label";

    val yleft = new PlainAxis("Left Axis");
    yleft.setRangeU(yrange);
    yleft.setLocationU(new Point2D.Double(xrange.start, yrange.start));
    yleft.setLabelFont(xbfont);
    val ytitle = new SGLabel("yaxis title", yLabel, new Point2D.Double(0.0,
      0.0));
    val ytfont = new Font("Helvetica", Font.PLAIN, 14);
    ytitle.setFont(ytfont);
    ytitle.setHeightP(0.2);
    yleft.setTitle(ytitle);
    posGraph.addYAxis(yleft);
    /*
         * Create a PointAttribute for the display of the Collection of points. The
         * points will be red with the label at the NE corner and in blue.
         */

    val posAttr = new PointAttribute(2, Color.red);
    posAttr.setLabelPosition(PointAttribute.NE);
    val pfont = new Font("Helvetica", Font.PLAIN, 12);
    posAttr.setLabelFont(pfont);
    posAttr.setLabelColor(Color.blue);
    posAttr.setLabelHeightP(0.1);
    posAttr.setMarkHeightP(0.15);
    val negAttr = new PointAttribute(50, Color.blue);
    negAttr.setLabelPosition(PointAttribute.NE);
    negAttr.setLabelFont(pfont);
    negAttr.setLabelColor(Color.blue);
    negAttr.setLabelHeightP(0.1);
    negAttr.setMarkHeightP(0.15);
    /*
         * Associate the attribute and the point Collection with the graph.
         */
    posGraph.setData(pos, posAttr);
    negGraph.setData(neg, negAttr);

    mainPane;
  }

}