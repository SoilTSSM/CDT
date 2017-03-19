
RHtests_license <- function(){
	UA1 <- 'RClimDex and RHtests software packages (all versions included), herein after called "The Library" \n'
	UA2 <- '©  Copyright, Environment Canada, 2012, \n'
	UA3 <- "The Library was created by the Climate Research Division of Environment Canada and as such all intellectual property rights (including copyright) that may exist in The Library are owned by the Government of Canada.  The Library software code is provided free under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, version 3.0 of the License. It is distributed under the terms of this license 'as-is' and has not been designed or prepared to meet any Licensee's particular requirements. Environment Canada makes no warranty, either express or implied, including but not limited to, warranties of merchantability or fitness for a particular purpose. In no event will Environment Canada be liable for any indirect, special, consequential or other damages attributed to the Licensee's use of The Library. In downloading The Library you understand and agree to these terms and those of the associated LGP License. See the GNU Lesser General Public License for more details.\n"
	UA4 <- "You should have received a copy of the GNU Lesser General Public License along with This Library; if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA."
	UserAgrement <- paste(UA1, UA2, UA3, UA4, sep = '\n')

	########################
	topFrame <- tktoplevel(pady = 2, bg = "gray94")
	tkgrab.set(topFrame)
	tkfocus(topFrame)

	outFrame <- tkframe(topFrame, padx = 2, bg = "gray94")
	buttonFrame <- tkframe(topFrame, padx = 2, bg = "gray94")

	########################

	scrollBar <- tkscrollbar(outFrame, repeatinterval = 5, command = function(...) tkyview(textFrame, ...))
	textFrame <- tktext(outFrame, bg = "gray94", font = "courier", yscrollcommand = function(...) tkset(scrollBar, ...))
	tkgrid(textFrame, scrollBar)
	tkgrid.configure(scrollBar, sticky = "ns")

	tcl("update", "idletasks")
	tkinsert(textFrame, "end", paste(UserAgrement, sep = ""))
	tkconfigure(textFrame, state = "disabled")

	########################
	retVal <- FALSE

	yesButton <- tkbutton(buttonFrame, text = "I Agree", command = function(){
		retVal <<- TRUE
		tkgrab.release(topFrame)
		tkdestroy(topFrame)
	})
	noButton <- tkbutton(buttonFrame, text = "I Do Not Agree", command = function(){
		tkgrab.release(topFrame)
		tkdestroy(topFrame)
	})
	tkgrid(yesButton, noButton)

	########################
	tkgrid(outFrame)
	tkgrid(buttonFrame)
	tkgrid.configure(outFrame, sticky = "nsew")

	tkwm.withdraw(topFrame)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", topFrame))
	tt.h <- as.integer(tkwinfo("reqheight", topFrame))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(topFrame, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(topFrame)
	tkwm.title(topFrame, "RHtestsV4 User Agreement")
	tkwm.deiconify(topFrame)

	tkfocus(topFrame)
	tkbind(topFrame, "<Destroy>", function() {
		tkgrab.release(topFrame)
	})
	tkwait.window(topFrame)
	return(retVal)
}


