#[allow(dead_code, unused, unused_variables, unused_imports)]

/**
###############################################################################
#  AST visualizer - generates a DOT file for Graphviz.                        #
#                                                                             #
#  To generate an image from the DOT file run $ dot -Tpng -o ast.png ast.dot  #
#                                                                             #
###############################################################################
 */
pub mod gen_ast_dot {
    use std::cell::RefCell;
    use std::mem;
    use std::rc::Rc;

    use crate::spi12::pascal_parser::*;

    pub struct ASTVisualizer {
        node_count: usize,
        pub dot_body: Vec<String>,
    }

    impl ASTVisualizer {
        pub fn new() -> Self {
            let mut dot_body = vec!["digraph astgraph {
    node [shape=circle, fontsize=12, fontname=\"Courier\", height=.1];
    ranksep=.3;
    edge [arrowsize=.5]\n\n"
                .to_string()];
            ASTVisualizer {
                node_count: 1,
                dot_body,
            }
        }

        fn visit(&mut self, node: Rc<RefCell<ASTTree>>) {
            let stat = node.as_ref().borrow().stat.clone();

            match stat {
                Statements::Binop(_) => {
                    self.visit_binop(Rc::clone(&node));
                }
                Statements::Assign(_) => {
                    self.visit_assign(Rc::clone(&node));
                }
                Statements::Num { .. } => {
                    self.visit_num(Rc::clone(&node));
                }
                Statements::UnaryOp { .. } => {
                    self.visit_unary(Rc::clone(&node));
                }
                Statements::Compound { .. } => {
                    self.visit_compound(Rc::clone(&node));
                }
                Statements::NoOp => {
                    self.visit_noop(Rc::clone(&node));
                }
                Statements::Var { .. } => {
                    self.visit_var(Rc::clone(&node));
                }
                Statements::Program { .. } => {
                    self.visit_program(node);
                }
                Statements::Block { .. } => {
                    self.visit_block(Rc::clone(&node));
                }
                Statements::VarDecl { .. } => {
                    self.visit_var_decl(Rc::clone(&node));
                }
                Statements::ProcedureDecl { .. } => {
                    self.visit_procedure_decl(Rc::clone(&node));
                }
                Statements::Type(_) => {
                    self.visit_type(Rc::clone(&node));
                }

                _ => {}
            }
        }

        fn visit_program(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!("    node{} [label=\"Program\"]\n", self.node_count.clone());
            self.dot_body.push(s);

            node.borrow_mut()._num = self.node_count;

            self.node_count += 1;

            if let Statements::Program { name, block } = node.borrow().stat.clone() {
                self.visit(Rc::clone(&block));
                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    block.borrow()._num
                );
                self.dot_body.push(s);
            }
        }

        fn visit_block(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!("    node{} [label=\"Block\"]\n", self.node_count);
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Statements::Block {
                declaration,
                compound_statement,
            } = node.borrow().stat.clone()
            {
                declaration.borrow().iter().for_each(|n| {
                    self.visit(Rc::clone(n));
                });
                self.visit(Rc::clone(&compound_statement));

                declaration.borrow().iter().for_each(|n| {
                    let s = format!(
                        "    node{} -> node{}\n",
                        node.borrow()._num,
                        n.borrow()._num
                    );
                    self.dot_body.push(s);
                });

                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    compound_statement.borrow()._num
                );
                self.dot_body.push(s);
            }
        }

        fn visit_var_decl(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!("    node{} [label=\"VarDecl\"]\n", self.node_count);
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Statements::VarDecl {
                var_node,
                type_node,
            } = node.borrow().stat.clone()
            {
                self.visit(Rc::clone(&var_node));
                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    var_node.borrow()._num
                );
                self.dot_body.push(s);

                self.visit(Rc::clone(&type_node));
                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    type_node.borrow()._num
                );
                self.dot_body.push(s);
            }
        }

        fn visit_procedure_decl(&mut self, node: Rc<RefCell<ASTTree>>) {
            let stat = node.as_ref().borrow().stat.clone();
            if let Statements::ProcedureDecl {
                proc_name,
                block_node,
            } = stat
            {
                let s = format!(
                    "    node{} [label=\"ProcDecl:{}\"]\n",
                    self.node_count,
                    proc_name.get_value()
                );
                self.dot_body.push(s);

                node.borrow_mut()._num = self.node_count;
                self.node_count += 1;
                self.visit(Rc::clone(&block_node));
                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    block_node.borrow()._num
                );
                self.dot_body.push(s);
            }
        }

        fn visit_type(&mut self, node: Rc<RefCell<ASTTree>>) {
            let type_name = node.borrow().stat.clone().get_token().get_value();
            let s = format!("    node{} [label=\"{}\"]\n", self.node_count, type_name);
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        fn visit_num(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count,
                node.borrow().stat.get_num()
            );

            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        fn visit_binop(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count,
                node.borrow().stat.get_token().token_type()
            );
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            self.visit(Rc::clone(&node.take().left.unwrap()));
            self.visit(Rc::clone(&node.take().right.unwrap()));

            let s1 = format!(
                "    node{} -> node{}\n",
                node.borrow()._num,
                node.take().left.unwrap().borrow()._num
            );
            let s2 = format!(
                "    node{} -> node{}\n",
                node.borrow()._num,
                node.take().right.unwrap().borrow()._num
            );
            self.dot_body.push(s1);
            self.dot_body.push(s2);
        }

        fn visit_unary(&mut self, node: Rc<RefCell<ASTTree>>) {
            if let Statements::UnaryOp { token, expr } = node.borrow().stat.clone() {
                let s = format!(
                    "    node{} [label=\"unary {}\"]\n",
                    self.node_count,
                    token.get_value()
                );
                self.dot_body.push(s);
                node.borrow_mut()._num = self.node_count;
                self.node_count += 1;

                self.visit(Rc::clone(&expr));
                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    expr.borrow()._num
                );
                self.dot_body.push(s);
            };
        }

        fn visit_compound(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!("    node{} [label=\"Compound\"]\n", self.node_count);
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Statements::Compound { children } = node.borrow().stat.clone() {
                children.borrow().iter().for_each(|child| {
                    self.visit(Rc::clone(child));
                    let s = format!(
                        "    node{} -> node{}\n",
                        node.borrow()._num,
                        child.borrow()._num
                    );
                    self.dot_body.push(s);
                });
            };
        }

        fn visit_assign(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count,
                node.borrow().stat.get_token().get_value()
            );
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Some(left) = node.borrow().left.clone() {
                self.visit(left.clone());
                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    left.borrow()._num
                );
                self.dot_body.push(s);
            }

            if let Some(right) = node.borrow().right.clone() {
                self.visit(right.clone());
                let s = format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    right.borrow()._num
                );
                self.dot_body.push(s);
            }
        }

        fn visit_var(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count,
                node.borrow().stat.get_token().get_value()
            );
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        fn visit_noop(&mut self, node: Rc<RefCell<ASTTree>>) {
            let s = format!("    node{} [label=\"NoOp\"]\n", self.node_count);
            self.dot_body.push(s);
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        pub fn _visit(&mut self, node: ASTTree) -> Vec<String> {
            let n = Rc::new(RefCell::new(node));
            self.visit(n);
            self.dot_body.clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{Read, Write};
    use std::process::Command;

    use crate::spi12::pascal_parser::*;

    use super::gen_ast_dot::*;

    #[test]
    fn test_gen_ast_dot() {
        if let Ok(mut file) = File::open("./part12.pas") {
            let mut input = String::new();
            file.read_to_string(&mut input).expect("can't read file.");

            let tree = Parser::new(Lexer::new(input)).parser();

            let mut output = ASTVisualizer::new()._visit(tree);
            output.push("}\n".to_string());
            let output = output.join("");

            let dot_file_name = "ast.dot";
            let path = "./";
            let dot_file_path = format!("{}{}", path, dot_file_name);
            let dot_file = File::create(dot_file_path);
            if let Ok(mut f) = dot_file {
                f.write_all(output.as_ref()).unwrap();
            }

            let mut dot = Command::new("dot");
            let _ = dot
                .args([
                    "-Tpng",
                    "-o",
                    &format!("{}ast.png", path),
                    &format!("{}ast.dot", path),
                ])
                .status();
        }
    }
}
