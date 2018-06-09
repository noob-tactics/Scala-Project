/* Author : KUMAR PRABUDDH */
/* 3D CUBE WIREFRAME */
/* USING SCALA */


import swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import event._
import scala.math._
import java.awt.Polygon
object Cube extends SimpleSwingApplication { 
  

    val top = new MainFrame {
	        title = "3D RotatingCube Wireframe"
		   	var nodeSize = 8
		   	// initializing nodes
			var node0 = (-100.0, -100.0, -100.0)
			var node1 = (-100.0, -100.0, 100.0)
			var node2 = (-100.0, 100.0, -100.0)
			var node3 = (-100.0, 100.0, 100.0)
			var node4 = (100.0, -100.0, -100.0)
			var node5 = (100.0, -100.0, 100.0)
			var node6 = (100.0, 100.0, -100.0)
			var node7 = (100.0, 100.0, 100.0)
			var nodes = Array(node0, node1, node2, node3, node4, node5, node6, node7)
			// initializing edges
			var edge0  = (0, 1)
			var edge1  = (1, 3)
			var edge2  = (3, 2)
			var edge3  = (2, 0)
			var edge4  = (4, 5)
			var edge5  = (5, 7)
			var edge6  = (7, 6)
			var edge7  = (6, 4)
			var edge8  = (0, 4)
			var edge9  = (1, 5)
			var edge10 = (2, 6)
			var edge11 = (3, 7)
			var edges = Array(edge0, edge1, edge2, edge3, edge4, edge5, edge6, edge7, edge8, edge9, edge10, edge11)
			//initializing faces
			var face0 = (0, 1, 2, 3)
			var face1 = (0, 1, 4, 5)
			var face2 = (0, 2, 4, 6)
			var face3 = (1, 3, 5, 7)
			var face4 = (2, 3, 6, 7)
			var face5 = (4, 5, 6, 7)
			var faces = Array(face0, face1, face2, face3, face4, face5)

			var colors = Array(Color.black, Color.white, Color.darkGray, Color.MAGENTA, Color.blue, Color.green)
			// rotation logic
			def rotateY( theta: Double) : Unit = {
	            var sinTheta = sin(theta)
	            var cosTheta = cos(theta)
	            
	            for (n <- 0 to (nodes.length - 1)) {
	                var node = nodes(n)
	                var x = node._1
	                var y = node._2
	                var z = node._3
	                var a = x * cosTheta - z * sinTheta
	                var b = z * cosTheta + x * sinTheta
	                nodes(n) = (a,y,b)
	            }
	        }

	        def rotateX( theta: Double) : Unit = {
	            var sinTheta = sin(theta)
	            var cosTheta = cos(theta)
	            
	            for (n <- 0 to (nodes.length - 1)) {
	                var node = nodes(n)
	                var x = node._1
	                var y = node._2
	                var z = node._3
	                var a = y * cosTheta - z * sinTheta
	                var b = z * cosTheta + y * sinTheta
	                nodes(n) = (x,a,b)
	            }
	        }

	        def rotateZ( theta: Double) : Unit = {
	            var sinTheta = sin(theta)
	            var cosTheta = cos(theta)
	            
	            for (n <- 0 to (nodes.length - 1)) {
	                var node = nodes(n)
	                var x = node._1
	                var y = node._2
	                var z = node._3
	                var a = x * cosTheta - y * sinTheta
	                var b = y * cosTheta + x * sinTheta
	                nodes(n) = (a,b,z)
	            }
	        }


	        rotateZ(30)
	        rotateY(20)
	        rotateX(20)
	       

			val drawPanel = new Panel {
	  	 				// defining graphics object
						override def paint(g:Graphics2D){
				  	 	    super.paintComponent(g)
				  			val backGroundColor = new Color(255, 255, 255)
						    val edgeColor = new Color(34, 68, 204)	
						    val nodeColor = new Color(40, 168, 107)	
						    val faceColor = new Color(205, 201, 201)
							g.translate(200, 200)
						    g.setBackground(backGroundColor)
						    g.setColor(edgeColor)
						    var e = 0
						    for (e <- edges) {
						        var n0 = e._1
						        var n1 = e._2
						        var node0 = nodes(n0)
						        var node1 = nodes(n1)
						        g.drawLine(node0._1.toInt, node0._2.toInt, node1._1.toInt, node1._2.toInt)
						    }
					    
						    // Draw nodes
						    g.setColor(nodeColor)
						    
						    var n = 0
						    for ( n <- nodes) {
						        var node = n
						        g.fillOval((node._1-nodeSize/2).toInt, (node._2-nodeSize/2).toInt, nodeSize, nodeSize)
						    }
					    	// filling color in faces
						    var Z = 0.0;
							var minNode = (0.0,0.0,0.0)
							var flag = Array(1,1,1,1,1,1)
							for(node <- nodes){
								if(node._3 < Z){
									Z = node._3
									minNode = node
								}
							}

							for(i <- 0 to faces.length - 1){
								if(nodes(faces(i)._1) == minNode || nodes(faces(i)._2) == minNode || nodes(faces(i)._3) == minNode || nodes(faces(i)._4) == minNode){
									flag(i) = 0
								}
							}

							for(i <- 0 to faces.length-1){
								if(flag(i)!=0){


							    var node1 = nodes(faces(i)._1)
								var node2 = nodes(faces(i)._2)
								var node3 = nodes(faces(i)._3)
								var node4 = nodes(faces(i)._4)
								var x = Array(node1._1.toInt, node2._1.toInt, node4._1.toInt, node3._1.toInt)
								var y = Array(node1._2.toInt, node2._2.toInt, node4._2.toInt, node3._2.toInt)
								g.setColor(colors(i))
								g.fillPolygon(x, y, 4)
						    
						    }
						 }   

			 	 	}		
			 	 	// event listener  	 
  	 				 listenTo(mouse.clicks , mouse.moves)
  		             var previousPoint = (0,0)
  		             var isYSpeedNegative = false
  		             var isXSpeedNegative = false
  		             var xSpeed = 0.0
  		             var ySpeed = 0.0
  		             var Xfinal = 0.0
  		             var Yfinal = 0.0
  		             reactions += {
		  	 		 case e: MousePressed => 
		  	 		 	var x = e.point.x - 200
		  	 		 	var y = e.point.y - 200
		  	 			previousPoint = (x,y)
		  	 			
		  	 		 case e: MouseDragged =>
		  	 		 	var x = e.point.x - 200
		  	 		 	var y = e.point.y - 200
		  	 		 	var thetaX = x - previousPoint._1
		  	 		 	var thetaY = y - previousPoint._2
		  	 		 	rotateX(thetaY/5000.0)
		  	 			rotateY(thetaX/5000.0)
		  	 			repaint
		  	 		 case e: MouseReleased =>
		  	 		 	
			  	 		 Xfinal = (e.point.x)-200
			  	 		 Yfinal = (e.point.y)-200	
		  	 		 	 xSpeed = (Xfinal - previousPoint._1)/500.0
		  	 		 	 ySpeed = (Yfinal - previousPoint._2)/500.0
		  	 		 	
		  	 		 	if(xSpeed >0){
		  	 		 		isXSpeedNegative = false
		  	 		 	}
		  	 		 	else{
		  	 		 		isXSpeedNegative = true
		  	 		 	}

		  	 		 	if(ySpeed>0){
		  	 		 		isXSpeedNegative = false
		  	 		 	}
    			 	 	else{
    			 	 		isYSpeedNegative = true
    			 	 	 }
    			 	 		timer.start
  			 	 
    			 	 }

    			 	 
  	 			 			def stopTimer() : Unit = {
  	 			 					timer.stop
  	 			 					}

					val timer = new javax.swing.Timer(10, Swing.ActionListener(e => {
							rotateY(xSpeed)
							rotateX(ySpeed)
							repaint
							if(isXSpeedNegative){
								xSpeed += 0.01
								if(xSpeed>0){
									stopTimer
								}	
							}
							else{
								xSpeed -= 0.01
								if(xSpeed<0){
									stopTimer
								}
							}					
							if(isYSpeedNegative){
								ySpeed += 0.01
								if(ySpeed>0){
									stopTimer
								}	
							}
							else{
								ySpeed -= 0.01
								if(ySpeed<0){
									stopTimer
								}
							}
						}))			

				 preferredSize = new Dimension(400,400)
			 } 

		 	

	     contents = new BorderPanel {
	    	layout (drawPanel) = Center 
	       } 
    		
    } 
 }

