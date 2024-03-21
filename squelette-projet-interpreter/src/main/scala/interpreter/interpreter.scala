package interpreter

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 * 
 * ETUDIANT 1 : ISOREZ Oscar
 * 
 * ETUDIANT 2 :
 * 
 */

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  // TODO TP2
  def lookUp(v: Variable, mem: Memory): Value = 
    mem match {
      case (variable, value) :: tail if variable == v => value
      case _ :: tail => lookUp(v, tail)
      case Nil => NlValue

    }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire modifiée par l'affectation [v->d]
   */
  // TODO TP2
  def assign(v: Variable, d: Value, mem: Memory): Memory = 
    (v, d) :: mem.filterNot(_._1 == v)

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  // TODO TP2
  def interpreterExpr(expression: Expression, mem: Memory): Value = 
    expression match {
      case Nl => NlValue
      case Cst(name) => CstValue(name)
      case VarExp(name) => lookUp(Var(name), mem)
      case Cons(arg1, arg2) => ConsValue(interpreterExpr(arg1, mem), interpreterExpr(arg2, mem))
      case Hd(arg) => interpreterExpr(arg, mem) match {
        case ConsValue(arg1, arg2) => arg1
        case _ => throw new Exception("Hd expects a ConsValue")
      }
      case Tl(arg) => interpreterExpr(arg, mem) match {
        case ConsValue(arg1, arg2) => arg2
        case _ => throw new Exception("Tl expects a ConsValue")
      }
      case Eq(arg1, arg2) => (interpreterExpr(arg1, mem), interpreterExpr(arg2, mem)) match {
        case (CstValue(name1), CstValue(name2)) => if (name1 == name2) CstValue("true") else CstValue("false")
        case (ConsValue(arg11, arg12), ConsValue(arg21, arg22)) => if (arg11 == arg21 && arg12 == arg22) CstValue("true") else CstValue("false")
        case _ => throw new Exception("Eq expects two CstValue or two ConsValue")
      }
    }


  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant l'expression de cette valeur
   */
  // TODO TP2
  def valueToExpression(value: Value): Expression = 
    value match {
      case NlValue => Nl
      case CstValue(name) => Cst(name)
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1), valueToExpression(arg2))
    }
  

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  // TODO TP2
  def interpreterCommand(command: Command, memory: Memory): Memory = 
    command match {
      case Nop => memory
      case Set(variable, expression) => assign(variable, interpreterExpr(expression, memory), memory)
      case While(condition, body) => interpreterCommand(While(condition, body), interpreterCommands(body, memory))
      case For(count, body) => interpreterCommand(For(count, body), interpreterCommands(body, memory))
      case If(condition, then_commands, else_commands) => interpreterCommand(If(condition, then_commands, else_commands), interpreterCommands(then_commands, memory))
    }
  
  
  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  // TODO TP2
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = 
    commands match {
      case command :: tail => interpreterCommands(tail, interpreterCommand(command, memory))
      case Nil => memory
    }
  

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  // TODO TP2
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = 
    vars.zip(vals).map{case (v, d) => (v, d)}
  

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = 
    vars.map(v => lookUp(v, memory))

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreter(program: Program, vals: List[Value]): List[Value] =
    program match {
      case Progr(in, body, out) => interpreterMemoryGet(out, interpreterCommands(body, interpreterMemorySet(in, vals)))
    }









    

    
  
  

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

}