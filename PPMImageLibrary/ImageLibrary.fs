module PPMImageLibrary

#light
//
// F#-based PPM image library.
//
// <<Jakub Glebocki>>
// U. of Illinois, Chicago
// CS341, Spring 2018
// Project 06
//

//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage (image:(int*int*int) list list) = 
  match image with
  | [] -> printfn "**END**"
  | hd::tl -> printfn "%A" hd
              OutputImage tl
           
let DebugOutput(width:int, height:int, depth:int, image:(int*int*int) list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage image


//
// TransformFirstThreeRows:
//
// An example transformation: replaces the first 3 rows of the given image
// with a row of Red, White and Blue pixels (go USA :-).
//
let rec BuildRowOfThisColor row color = 
  match row with
  | []     -> []
  | hd::tl -> color :: BuildRowOfThisColor tl color

let TransformFirstThreeRows(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let row1 = List.head image
  let row2 = List.head (List.tail image)
  let row3 = List.head (List.tail (List.tail image))
  let tail = List.tail (List.tail (List.tail image))
  let newRow1 = BuildRowOfThisColor row1 (255,0,0)      // red:
  let newRow2 = BuildRowOfThisColor row2 (255,255,255)  // white:
  let newRow3 = BuildRowOfThisColor row3 (0,0,255)      // blue:
  let newImage = newRow1 :: newRow2 :: newRow3 :: tail
  newImage


//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let Flatten (SL:string list) = 
  List.reduce (fun s1 s2 -> s1 + " " + s2) SL

let Image2ListOfStrings (image:(int*int*int) list list) = 
  List.map (fun TL -> List.map (fun (r,g,b) -> r.ToString()+" "+g.ToString()+" "+b.ToString()+" ") TL) image
  |> List.map Flatten

let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = [ "P3" ] @ 
          [ System.Convert.ToString(width); System.Convert.ToString(height) ] @
          [ System.Convert.ToString(depth) ] @
          (Image2ListOfStrings image)
  System.IO.File.WriteAllLines(filepath, L)
  true  // success



//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//

//let AvgOfTuples image =
//  match image with
//  | [] -> 0
//  | (a,b,c)::tail -> (a+b+c)/3



//Recursive function to add up and divide by 3
let rec _Grayscale row =
  match row with
  | [] -> []
  | (a,b,c)::tail -> (((a+b+c)/3),((a+b+c)/3),((a+b+c)/3))::(_Grayscale tail)

//Using list.map to access each tuple,
let rec Grayscale(width:int, height:int, depth:int, image:(int*int*int) list list) = 
   List.map(fun x-> _Grayscale x) image
   

//
// Threshold
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//

//Function check threshold and returns value
let PixelCheck x thres =
  if x <= thres then (0) else (255)

  //Recursive function to check pixel and assign either 0 or 255 depending on threshold
let rec _Threshold row threshold=
  match row with
  [] -> []
  |(a,b,c)::tail -> ((PixelCheck a threshold),(PixelCheck b threshold),(PixelCheck c threshold))::(_Threshold tail threshold)


  //Using list.map to access each tuple,
let rec Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = 
  List.map(fun x-> _Threshold x threshold) image


//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row.
//

//recursive function, calling tail recrusively and concatenating tuple to new "acc" list
let rec _FlipHorizontal image acc =
  match image with
  | [] -> acc          //Don't do List.rev here because it would reverse the reverse. we want the reverse.
  | (a,b,c)::tail -> _FlipHorizontal tail ((a,b,c)::acc)

   //Using list.map to access each tuple,
let rec FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  List.map(fun x-> _FlipHorizontal x []) image
  
  

//
// Zoom:
//
// Zooms the image by the given zoom factor, which is an integer 0 < factor < 5.  
// The function uses the nearest neighbor approach where each pixel P in the original 
// image is replaced by a factor*factor block of P pixels.  For example, if the zoom 
// factor is 4, then each pixel is replaced by a 4x4 block of 16 identical pixels. 
// The nearest neighbor algorithm is the simplest zoom algorithm, but results in 
// jagged images.  The resulting image is returned.
//

//let __Zoom a b c factor =
//  if factor = 4 then [(a,b,c); (a,b,c); (a,b,c); (a,b,c) ]
//  else if factor = 3 then [(a,b,c);(a,b,c);(a,b,c) ]
//  else if factor = 2 then [(a,b,c);(a,b,c)]
//  else [(a,b,c)]

//Function to deal with which case to zoom into
let __Zoom one two three factor =
  match factor with
  | 1 -> [(one,two,three) ]
  | 2 -> [(one,two,three);(one,two,three) ]
  | 3 -> [(one,two,three);(one,two,three);(one,two,three) ]
  | 4 -> [(one,two,three); (one,two,three); (one,two,three);(one,two,three) ]
  | _ -> [(one,two,three)]
  
  //Recursive function to zoom on each tuple, replicate it, and concatenate it
let rec _Zoom row factor listAccumulated =
  match row with
  | [] -> List.rev listAccumulated
  |(a,b,c)::tail -> let tuple = __Zoom a b c factor
                    _Zoom tail factor (tuple @ listAccumulated)


   //Using list.map to access each tuple,
let rec Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
  List.map(fun x -> _Zoom x factor []) image



//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//


//recursive function taking in image and a new image2
let rec _RotateRight90 image image2 =
   //Getting the FirstRow
  let RowOneOfList = List.head image
  //Matching that row with
  match RowOneOfList with
  | [] -> image2   //If empty return image2

  | _ -> let firstcolumn = List.map(fun x -> List.head x) image     //else get first column
         
         let secondcolumn = List.map(fun x -> List.tail x) image    // get the tail
         
         let columnRev = List.rev firstcolumn                       //and reverse the column
        
         _RotateRight90 secondcolumn (columnRev::image2)               //Finally concatenate while doing a recursive call

  //Doing list.rev to be able to reverse it each time 90 degrees in the correct orientation
let rec RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  List.rev (_RotateRight90 image [])
