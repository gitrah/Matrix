package com.hartenbower.matrix.view
import javax.swing._
import com.hartenbower.matrix.MatrixD
import java.awt.BorderLayout
import java.awt.FlowLayout
import java.awt.MediaTracker
import java.awt.Color
import java.awt.Font
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

object SgtMViewer {
    def graph(x:MatrixD, y:MatrixD) {
        val pd = new SgtMViewer
        val frame = new JFrame("Point Demo");
        val button = new JPanel();
        var graph : JPane = null;
        button.setLayout(new FlowLayout());
        pd.tree_ = new JButton("Tree View");
        val myAction = new pd.MyAction();
        pd.tree_.addActionListener(myAction);
        button.add(pd.tree_);
        pd.space_ = new JButton("Add Mark");
        pd.space_.addActionListener(myAction);
        button.add(pd.space_);
        frame.getContentPane().setLayout(new BorderLayout());
        frame.addWindowListener(new java.awt.event.WindowAdapter() {
            override def  windowClosing( event : java.awt.event.WindowEvent) {
                val fr =  event.getSource().asInstanceOf[JFrame];
                fr.setVisible(false);
                fr.dispose();
                System.exit(0);
            }
        });
        frame.setSize(553, 438);
        graph = pd.makeGraph(x,y);
        graph.setBatch(true);
        frame.getContentPane().add(graph, BorderLayout.CENTER);
        frame.getContentPane().add(button, BorderLayout.SOUTH);
        frame.pack();
        frame.setVisible(true);
        graph.setBatch(false);
    }
}
class SgtMViewer extends JApplet {
  
    def tree_actionPerformed( e : java.awt.event.ActionEvent) {
        val ct = new JClassTree()
        ct.setModal(false)
        ct.setJPane(mainPane_)
        ct.show()
    }

    class MyAction extends java.awt.event.ActionListener {
        def actionPerformed(event: java.awt.event.ActionEvent ) {
            val obj = event.getSource();
            if (obj == space_) {
                System.out.println("  <<Mark>>");
            }
            if (obj == tree_)
                tree_actionPerformed(event);
        }
    }
    var tree_ : JButton = null
    var space_  : JButton = null
    var mainPane_ : JPane = null



  def graph(x:MatrixD, y: MatrixD, pointFn : (Double)=>Int) {}
  
    def makeGraph(x:MatrixD,y: MatrixD) :JPane={
        /*
         * This example creates a very simple plot from scratch (not using one of
         * the sgt.awt classes) to display a Collection of points.
         */
        /*
         * Create a Pane, place in the center of the Applet and set the layout to be
         * StackedLayout.
         */
        mainPane_ = new JPane("Point Plot Demo", new Dimension(553, 438));
        mainPane_.setLayout(new StackedLayout());
        mainPane_.setBackground(Color.white);
        /*
         * Create a Collection of points using the TestData class.
         */
        val (min,max) = x.range()
        val xrange = new Range2D(50.0, 150., 10.0);
        val yrange = new Range2D(-20.0, 20.0, 5.0);
        val td = new TestData(xrange, yrange, 50);
        val col = td.getCollection();
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
        val layer = new Layer("Layer 1", new Dimension2D(xsize, ysize));
        mainPane_.add(layer);
        /*
         * create and add image as a Logo to the layer
         */
        val img = null;// this.getToolkit().getImage( getClass().getResource("ncBrowse48.gif"));
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
        layer.addChild(logo);
        /*
         * Create a CartesianGraph and transforms.
         */

        val graph = new CartesianGraph("Point Graph");
        layer.setGraph(graph);
        val xt = new LinearTransform(xstart, xend, xrange.start, xrange.end);
        val yt = new LinearTransform(ystart, yend, yrange.start, yrange.end);
        graph.setXTransform(xt);
        graph.setYTransform(yt);
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
        graph.addXAxis(xbot);
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
        graph.addYAxis(yleft);
        /*
         * Create a PointAttribute for the display of the Collection of points. The
         * points will be red with the label at the NE corner and in blue.
         */

        val pattr = new PointAttribute(20, Color.red);
        pattr.setLabelPosition(PointAttribute.NE);
        val pfont = new Font("Helvetica", Font.PLAIN, 12);
        pattr.setLabelFont(pfont);
        pattr.setLabelColor(Color.blue);
        pattr.setLabelHeightP(0.1);
        pattr.setDrawLabel(true);
        /*
         * Associate the attribute and the point Collection with the graph.
         */
        graph.setData(col, pattr);

        mainPane_;
    }

}