%To Start the system type start.


:- use_module(library(jpl)).
start :-
		
		
		interface2.
		
		
      
        
        
    conditions(Tree,high_temperature) :- verify(Tree,"do the area have high temperature (y/n) ?").
 
    conditions(Tree,medium_rainfall) :- verify(Tree,"do the area have medium rainfall (y/n) ?").
  
    conditions(Tree,absolute_humidity) :- verify(Tree,"do the area have absolute humidity (y/n) ?").

    conditions(Tree,clay_soil) :- verify(Tree,"do the area have clay soil (y/n) ?").
    
    conditions(Tree,medium_temeperature) :- verify(Tree,"do the area have medium temperature (y/n) ?").
    
    conditions(Tree,loam_soil) :- verify(Tree,"do the area have loam soil (y/n) ?").
	
    conditions(Tree,ph7) :- verify(Tree,"do the area have ph7 (y/n) ?").
 
    conditions(Tree,sandy_soil) :- verify(Tree,"do the area have sandy soil (y/n) ?").
   
    conditions(Tree,low_temperature) :- verify(Tree," do the area have low temperature (y/n) ?").
  
    conditions(Tree,relative_humidity) :- verify(Tree,"do the area have high humidity (y/n) ?").
   
    conditions(Tree,high_rainfall) :- verify(Tree," do the area have high rainfall (y/n) ?").
	conditions(Tree,high_capilarity) :- verify(Tree," do the area have high capilarity (y/n) ?").
	conditions(Tree,ph7_above) :- verify(Tree," do the area have ph7 and above (y/n) ?").
	conditions(Tree,dry_area) :- verify(Tree," is the area dry  (y/n) ?").
	
	/*conditions(_,"Sorry, I don't seem to help in selecting the tree.").*/

        
    hypothesis(Tree,gravelea) :-
    conditions(Tree,high_temperature),
    conditions(Tree,medium_rainfall),
    conditions(Tree,absolute_humidity),
    conditions(Tree,clay_soil).
        
    
    hypothesis(Tree,pine) :-
    conditions(Tree,medium_temeperature),
    conditions(Tree,loam_soil),
    conditions(Tree,absolute_humidity),
    conditions(Tree,ph7).
        
    hypothesis(Tree,cyprus) :-
    conditions(Tree,sandy_soil),
    conditions(Tree,ph7),
    conditions(Tree,low_temperature),
    conditions(Tree,relative_humidity).
            
        
    hypothesis(Tree,eucalyptus) :-
    conditions(Tree,high_rainfall),
    conditions(Tree,absolute_humidity),
    conditions(Tree,high_capilarity),
    conditions(Tree,ph7_above).
        
        
    hypothesis(Tree,elgon_tik) :-
    conditions(Tree,high_temperature),
    conditions(Tree,relative_humidity),
	conditions(Tree,high_rainfall).
    
    hypothesis(Tree,meruoak) :-
    conditions(Tree,dry_area).
        
    
    
        
	hypothesis(_,"unknown.").
	
    response(Reply) :-
        read(Reply),
        write(Reply),nl.
		
ask(Tree,Question) :-
	write(Tree),write(', '),write(Question),
	/*read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail),*/
	
	interface(', ',Tree,Question),
	 nl.
	
:- dynamic yes/1,no/1.		
	
verify(P,S) :-
   (yes(S) 
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(P,S))).
	 
undo :- retract(yes(_)),fail. 
undo :- retract(no(_)),fail.
undo.


pt(Tree):- 

		hypothesis(Tree,Plant),
		interface3(Tree,', you can plant this tree: ' ,Plant, '.'),
        write(Tree),write(', you can plant this tree: '),write(Plant), write('.'),undo,end.

end :-
		nl,nl,nl,
		sleep(0.0),
		write('*****************************************************************'),nl,
		sleep(0.0),
		write("################||| THANK YOU FOR USING ME |||#####################"),nl,
		sleep(0.0),
		write('*****************************************************************'),nl.

interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- TREE SELECTION EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setTitle, ['I help you in selecting trees based on the environmental conditions'], _),
	jpl_call(F, setSize, [700,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),
	jpl_call(F, dispose, [], _), 
	write(N),nl,
	( (N == yes ; N == y)
      ->
       assert(yes(Z)) ;
       assert(no(Z)), fail).
	   		
interface2 :-
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- TREE SELECTION EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [700,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hi am here to help you, First of all tell me the name of the area please'], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(null)
		->	write('you cancelled'),interface3('you cancelled. ','Thank you ','for use ','me.'),end,fail
		;	write("Hi. How are you? First of all tell me the name of the area please : "),write(N),nl,pt(N)
	).
	
	
interface3(P,W1,D,W2) :-
	atom_concat(P,W1, A),
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- TREE SELECTION EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [700,700], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(void)
		->	write('')
		;	write("")
	).
	
help :- write("To start the expert system please type 'start.' and press Enter key").