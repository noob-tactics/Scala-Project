// Rotate shape around the z-axis
def rotateY3D( theta: Double) : Unit = {
            var sin_t = sin(theta)
            var cos_t = cos(theta)
            
            for (n <- 0 to (nodes.length - 1)) {
                var node = nodes(n)
                var x = node._1
                var y = node._2
                var z = node._3
                var a = (x * cos_t - z * sin_t).toInt
                var b = (z * cos_t + x * sin_t).toInt
                nodes(n) = (a,y,b)
            }
        }

        def rotateX3D( theta: Double) : Unit = {
            var sin_t = sin(theta)
            var cos_t = cos(theta)
            
            for (n <- 0 to (nodes.length - 1)) {
                var node = nodes(n)
                var x = node._1
                var y = node._2
                var z = node._3
                var a = (y * cos_t - z * sin_t).toInt
                var b = (z * cos_t + y * sin_t).toInt
                nodes(n) = (x,a,b)
            }
        }

        def rotateZ3D( theta: Double) : Unit = {
            var sin_t = sin(theta)
            var cos_t = cos(theta)
            
            for (n <- 0 to (nodes.length - 1)) {
                var node = nodes(n)
                var x = node._1
                var y = node._2
                var z = node._3
                var a = (x * cos_t - y * sin_t).toInt
                var b = (y * cos_t + x * sin_t).toInt
                nodes(n) = (a,b,z)
            }
        }


        rotateZ3D(30)
        rotateY3D(20)
        rotateX3D(20)



mouseDragged = function() {
    rotateY3D(mouseX - pmouseX);
    rotateX3D(mouseY - pmouseY);
};