
##Text out

assign("xscr.out",tkscrollbar(out.frame, repeatinterval=5,orient="horizontal",
                       command=function(...)tkxview(main.txt.out,...)),envir=.GlobalEnv)
assign("yscr.out" ,tkscrollbar(out.frame, repeatinterval=5,
                       command=function(...)tkyview(main.txt.out,...)),envir=.GlobalEnv)
assign("main.txt.out" , tktext(out.frame,bg="white", #font=tkfont.create(family="courier",size=11),
    xscrollcommand=function(...)tkset(xscr.out,...),yscrollcommand=function(...)tkset(yscr.out,...),
wrap="none",height=txtHeight),envir=.GlobalEnv)

tkgrid(main.txt.out,yscr.out)
tkgrid(xscr.out)
tkgrid.configure(yscr.out,sticky="ns")
tkgrid.configure(xscr.out,sticky="ew")
tkgrid.configure(main.txt.out,sticky='nswe')
#tkgrid.rowconfigure(main.txt.out,0,weight=1)
tkgrid.columnconfigure(main.txt.out,0,weight=1)

