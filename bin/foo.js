<!-- begin hiding code from older browsers...
//
// The following JavaScript code is ORIGINAL and COPYRIGHT 1998 by MSE.
// NOTE: This code is NOT to be used for any commercial purposes, i.e.,
//	something for which you will be compensated in any form!
// This code is 'thank-you-ware'...
// 	it is NOT 'public domain'!!!
// If you wish to use this code or a derivitive of this code,
// 	please send me an email 'thank-you' to jsmaze@ellars.com
//	along with where you plan to use the code (I like to see what
//	other uses people can come up with...).
// If you use this code, please include the following information:
//
//	Original Code Copyright:	MSE, jsmaze@ellars.com
//	Original Code Date:		19980829 (MSE, ellars@scf.usc.edu)
//	Last Update:			20000101 (MSE, jsmaze@ellars.com)
//
// Thanks, and enjoy!
//
// maze image array - pre-loads sixteen maze images plus GO
var flip=new Array(17);
for(var i=0;i<17;i++)
	flip[i]=new Image();
	flip[0].src="rg00.gif";  // blank
	flip[1].src="rg01.gif";  // left
	flip[2].src="rg02.gif";  // right
	flip[3].src="rg03.gif";  // top
	flip[4].src="rg04.gif";  // bottom
	flip[5].src="rg05.gif";  // top-left
	flip[6].src="rg06.gif";  // top-right
	flip[7].src="rg07.gif";  // bottom-right
	flip[8].src="rg08.gif";  // bottom-left
	flip[9].src="rg09.gif";  // top-open
	flip[10].src="rg10.gif"; // right-open
	flip[11].src="rg11.gif"; // bottom-open
	flip[12].src="rg12.gif"; // left-open
	flip[13].src="rg13.gif"; // right-left
	flip[14].src="rg14.gif"; // top-bottom
	flip[15].src="rg15.gif"; // closed
	flip[16].src="rg-go.gif";// GO arrow
//
// let's do something wild... randomly generate a number to choose a
//	a layout for the maze...
var crapShoot = new Date();
var snakeEyes = crapShoot.getMilliseconds()%3;
//
// cellState array for 81 + 2 cells (body + start + finish)
// 	this array can be changed to re-arrange the maze... new nState
//	values are pulled from this array, so you only need to change THIS
//	array to update the entire maze!
//		(a future update will generate this array on the fly)
//		(right now it relies on the random number generated above)
if(snakeEyes == 0)
	{
	var cellState = new Array(16,
		10,11,5,14,3,10,6,6,12,
		5,12,0,13,6,13,4,7,13,
		13,3,1,4,0,1,3,14,2,
		8,1,14,0,2,2,14,7,6,
		11,2,0,2,0,5,14,9,13,
		13,8,7,8,4,3,7,0,7,
		5,10,5,3,7,11,6,4,13,
		8,0,7,1,14,0,6,3,7,
		9,7,4,7,8,14,12,8,10,
		16);
	}
//
if (snakeEyes == 1)
	{
	var cellState = new Array(16,
		5,11,3,6,5,10,3,14,6,
		13,8,2,1,7,9,13,4,2,
		1,2,14,3,11,8,7,3,7,
		13,8,3,0,7,3,0,1,11,
		10,6,9,3,6,13,8,4,12,
		8,2,5,7,13,14,5,14,2,
		1,4,0,0,3,6,10,6,13,
		5,3,7,6,1,12,5,7,2,
		10,9,12,7,8,14,4,12,10,
		16);
	}
//
if (snakeEyes == 2)
	{
	var cellState = new Array(16,
		11,5,6,10,14,6,5,3,12,
		13,9,1,3,8,4,5,0,13,
		1,14,2,9,1,6,13,14,2,
		13,11,8,6,7,1,7,14,11,
		5,7,5,9,13,10,11,13,2,
		8,2,1,12,0,6,8,4,7,
		10,2,0,3,7,1,0,14,13,
		13,11,9,11,12,4,6,3,7,
		8,14,7,10,14,9,7,8,10,
		16);
	}
//
// set initial values for nState, mod9, iPart
//	these three variables are the reason the maze code works...
nState=99;	// current state of the maze, that is, answer to the
		// question, what element type is the mouse over?
		// nState = 99 is the base value
mod9=0;		// modulo nine value
iPart=0;	// integer part value
//
// error function
//	each 'checker' function calls this function if there is an error
function errorCatch (cellLoc)
	{
	eval("document.cell"+cellLoc+".src=flip[15].src");
	nState=99;
	alert("Sorry, please try again.");
	eval("document.cell"+cellLoc+".src=flip[0].src");
	document.cell101.src=flip[16].src;
	document.cell88.src=flip[0].src;
	return true;
	}
//
// mouseOver function
//	this is the basic function that is called in the HTML code...
//	it should probably have a more exotic name, but oh well...
function whatever (cellLoc)
{
if (nState == 2000) clickGo();
else	{
if ((nState != 99 && cellLoc != 101) || (nState == 99 && cellLoc == 101))
		{
		eval("movedFrom"+nState+"("+cellLoc+")");
		}
	else	{
		if (cellLoc == 101)
			{
			nState=99;
			mod9=0;
			iPart=0;
			movedFrom99(101);
			}
		}
	}
}
//
// if user screws up (error - somehow manages to crash code)
//	Since I plugged a few security gaps, I'm not sure this function
//	is needed any longer... but we'll keep it around just for kicks...
function movedFromundefined (cellLoc)
	{
	movedFrom99(cellLoc);
	return true;
	}
//
// if nState=99 (error - standard)
function movedFrom99 (cellLoc)
	{
	document.cell101.src=flip[16].src;
	cellVal=cellLoc;
	mod9=0;
	iPart=0;
	nState=101;
	return true;
	}
//
// if nState=101 (user has just passed GO)
function movedFrom101 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if (cellVal == 1) 
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		document.cell101.src=flip[0].src;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=0
function movedFrom0 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	nState=cellState[cellVal];
	mod9=newMod9;
	iPart=newiPart;
	eval("document.cell"+cellVal+".src=flip[nState].src");
	return true;
	}
//
// if nState=1
function movedFrom1 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
if(newMod9 == mod9+1 || (newiPart == iPart+1 || newiPart == iPart-1))
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=2
function movedFrom2 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
if(newMod9 == mod9-1 || (newiPart == iPart+1 || newiPart == iPart-1))
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=3
function movedFrom3 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
if(newiPart == iPart+1 || (newMod9 == mod9+1 || newMod9 == mod9-1))
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=4
function movedFrom4 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
if(newiPart == iPart-1 || (newMod9 == mod9+1 || newMod9 == mod9-1))
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=5
function movedFrom5 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newMod9 == mod9+1 || newiPart == iPart+1)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=6
function movedFrom6 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newMod9 == mod9-1 || newiPart == iPart+1)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=parseInt(newiPart);
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=7
function movedFrom7 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newMod9 == mod9-1 || newiPart == iPart-1)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=8
function movedFrom8 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newMod9 == mod9+1 || newiPart == iPart-1)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=9
function movedFrom9 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newiPart == iPart-1 && newMod9 == mod9)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=10
function movedFrom10 (cellLoc)
	{
	if (cellLoc == 88 && mod9 == 9)
		{
		document.cell101.src=flip[0].src;
		document.cell88.src=flip[16].src;
		nState=2000;
		}
	else	{
		cellVal=parseInt(cellLoc);
		newMod9=cellVal%9;
		if (newMod9 == 0) newMod9=9;
		newiPart=parseInt((cellVal+8)/9);
		if(newMod9 == mod9+1)
			{
			nState=cellState[cellVal];
			mod9=newMod9;
			iPart=parseInt(newiPart);
			eval("document.cell"+cellVal+".src=flip[nState].src");
			}
		else	{
			errorCatch(cellLoc);
			}
		}
	return true;
	}
//
// if nState=11
function movedFrom11 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newiPart == iPart+1)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=12
function movedFrom12 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newMod9 == mod9-1 && newiPart == iPart)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=13
function movedFrom13 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newMod9 == mod9)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=14
function movedFrom14 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newiPart == iPart)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if nState=88
function movedFrom88 (cellLoc)
	{
	cellVal=parseInt(cellLoc);
	newMod9=cellVal%9;
	if (newMod9 == 0) newMod9=9;
	newiPart=parseInt((cellVal+8)/9);
	if(newiPart == iPart)
		{
		nState=cellState[cellVal];
		mod9=newMod9;
		iPart=newiPart;
		eval("document.cell"+cellVal+".src=flip[nState].src");
		}
	else	{
		errorCatch(cellLoc);
		}
	return true;
	}
//
// if you click on the second GO image after making it through the maze...
function clickGo ()
	{
	if(nState==2000)
		{
		document.location.href="../";
		}
	else	{
		if(nState==101)
			{
			document.cell1.src=flip[10].src;
			}
		else	{
			void(0);	// null function, does nothing
			}
		}
	return true;
	}
//
// end hide -->
