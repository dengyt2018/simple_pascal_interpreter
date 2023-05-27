#[allow(dead_code, unused, unused_variables, unused_imports)]

///
///
/// ```
/// ###############################################################################
///   AST visualizer - generates a DOT file for Graphviz.
///
///   To generate an image from the DOT file run $ dot -Tpng -o ast.png ast.dot
///
/// ###############################################################################
/// ```
///
pub mod gen_ast_dot {
    use crate::parser::{ASTTree, RefAST, Statements};
    use crate::rclone;
    use std::cell::RefCell;
    use std::rc::Rc;

    pub struct ASTVisualizer {
        node_count: usize,
        pub dot_body: Vec<String>,
    }

    impl ASTVisualizer {
        pub fn new() -> Self {
            ASTVisualizer {
                node_count: 1,
                dot_body: vec!["digraph astgraph {
    node [shape=circle, fontsize=12, fontname=\"Courier\", height=.1];
    ranksep=.3;
    edge [arrowsize=.5]\n\n"
                    .to_string()],
            }
        }

        fn visit(&mut self, node: RefAST) {
            let stat = node.as_ref().borrow().stat.clone();

            match stat {
                Statements::Binop(_) => {
                    self.visit_binop(rclone!(&node));
                }
                Statements::Assign(_) => {
                    self.visit_assign(rclone!(&node));
                }
                Statements::Num { .. } => {
                    self.visit_num(rclone!(&node));
                }
                Statements::String { value } => {
                    self.visit_string(rclone!(&node));
                }
                Statements::UnaryOp { .. } => {
                    self.visit_unary(rclone!(&node));
                }
                Statements::Compound { .. } => {
                    self.visit_compound(rclone!(&node));
                }
                Statements::NoOp => {
                    self.visit_noop(rclone!(&node));
                }
                Statements::Var { .. } => {
                    self.visit_var(rclone!(&node));
                }
                Statements::Program { .. } => {
                    self.visit_program(node);
                }
                Statements::Block { .. } => {
                    self.visit_block(rclone!(&node));
                }
                Statements::VarDecl { .. } => {
                    self.visit_var_decl(rclone!(&node));
                }
                Statements::ProcedureDecl { .. } => {
                    self.visit_procedure_decl(rclone!(&node));
                }
                Statements::Type(_) => {
                    self.visit_type(rclone!(&node));
                }
                Statements::Param { .. } => {
                    self.visit_param(rclone!(&node));
                }
                Statements::ProcedureCall { .. } => {
                    self.visit_procedure_call(rclone!(&node));
                }
                _ => {}
            }
        }

        fn visit_program(&mut self, node: RefAST) {
            self.dot_body
                .push(format!("    node{} [label=\"Program\"]\n", self.node_count));

            node.borrow_mut()._num = self.node_count;

            self.node_count += 1;

            if let Statements::Program { block, .. } = node.borrow().stat.clone() {
                self.visit(rclone!(&block));

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    block.borrow()._num
                ));
            }
        }

        fn visit_block(&mut self, node: RefAST) {
            self.dot_body
                .push(format!("    node{} [label=\"Block\"]\n", self.node_count));
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Statements::Block {
                declaration,
                compound_statement,
            } = node.borrow().stat.clone()
            {
                declaration.borrow().iter().for_each(|n| {
                    self.visit(rclone!(n));
                });
                self.visit(rclone!(&compound_statement));

                declaration.borrow().iter().for_each(|n| {
                    self.dot_body.push(format!(
                        "    node{} -> node{}\n",
                        node.borrow()._num,
                        n.borrow()._num
                    ));
                });

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    compound_statement.borrow()._num
                ));
            }
        }

        fn visit_var_decl(&mut self, node: RefAST) {
            self.dot_body
                .push(format!("    node{} [label=\"VarDecl\"]\n", self.node_count));

            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Statements::VarDecl {
                var_node,
                type_node,
            } = node.borrow().stat.clone()
            {
                self.visit(rclone!(&var_node));

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    var_node.borrow()._num
                ));

                self.visit(rclone!(&type_node));

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    type_node.borrow()._num
                ));
            }
        }

        fn visit_procedure_decl(&mut self, node: RefAST) {
            let stat = node.as_ref().borrow().stat.clone();
            if let Statements::ProcedureDecl {
                proc_name,
                params,
                block_node,
            } = stat
            {
                self.dot_body.push(format!(
                    "    node{} [label=\"ProcDecl:{}\"]\n",
                    self.node_count,
                    proc_name.get_value()
                ));
                node.borrow_mut()._num = self.node_count;
                self.node_count += 1;

                params.iter().for_each(|param| {
                    self.visit(rclone!(param));
                    self.dot_body.push(format!(
                        "    node{} -> node{}\n",
                        node.borrow()._num,
                        param.borrow()._num
                    ));
                });

                self.visit(rclone!(&block_node));

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    block_node.borrow()._num
                ));
            }
        }

        fn visit_param(&mut self, node: RefAST) {
            self.dot_body
                .push(format!("    node{} [label=\"Param\"]\n", self.node_count));

            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Statements::Param {
                var_name: var_node,
                var_type: type_node,
            } = node.borrow().stat.clone()
            {
                self.visit(rclone!(&var_node));

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    var_node.borrow()._num
                ));

                self.visit(rclone!(&type_node));

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    type_node.borrow()._num
                ));
            }
        }

        fn visit_procedure_call(&mut self, node: RefAST) {
            let (proc_name, actual_params) = node.borrow().stat.get_procedure_call();

            self.dot_body.push(format!(
                "    node{} [label=\"ProcCall:{}\"]\n",
                self.node_count,
                proc_name.get_value()
            ));

            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            actual_params.iter().for_each(|param| {
                self.visit(rclone!(param));
                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    param.borrow()._num
                ));
            });
        }

        fn visit_type(&mut self, node: RefAST) {
            let type_name = node.borrow().stat.clone().get_token().get_value();
            self.dot_body.push(format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count, type_name
            ));
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        fn visit_string(&mut self, node: RefAST) {
            let s = node.borrow().stat.get_string_object().get_string();

            self.dot_body
                .push(format!("    node{} [label=\"{}\"]\n", self.node_count, s));

            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        fn visit_num(&mut self, node: RefAST) {
            self.dot_body.push(format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count,
                node.borrow().stat.get_num()
            ));

            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        fn visit_binop(&mut self, node: RefAST) {
            self.dot_body.push(format!(
                "    node{} [label={:#?}]\n",
                self.node_count,
                node.borrow().stat.get_token().token_type()
            ));
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Some(left) = node.borrow().left.clone() {
                self.visit(left.clone());

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    left.borrow()._num
                ));
            }

            if let Some(right) = node.borrow().right.clone() {
                self.visit(right.clone());

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    right.borrow()._num
                ));
            }
        }

        fn visit_unary(&mut self, node: RefAST) {
            if let Statements::UnaryOp { token, expr } = node.borrow().stat.clone() {
                self.dot_body.push(format!(
                    "    node{} [label=\"unary {}\"]\n",
                    self.node_count,
                    token.get_value()
                ));
                node.borrow_mut()._num = self.node_count;
                self.node_count += 1;

                self.visit(rclone!(&expr));

                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    expr.borrow()._num
                ));
            };
        }

        fn visit_compound(&mut self, node: RefAST) {
            self.dot_body.push(format!(
                "    node{} [label=\"Compound\"]\n",
                self.node_count
            ));
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Statements::Compound { children } = node.borrow().stat.clone() {
                children.borrow().iter().for_each(|child| {
                    self.visit(rclone!(child));

                    self.dot_body.push(format!(
                        "    node{} -> node{}\n",
                        node.borrow()._num,
                        child.borrow()._num
                    ));
                });
            };
        }

        fn visit_assign(&mut self, node: RefAST) {
            self.dot_body.push(format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count,
                node.borrow().stat.get_token().get_value()
            ));
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;

            if let Some(left) = node.borrow().left.clone() {
                self.visit(left.clone());
                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    left.borrow()._num
                ));
            }

            if let Some(right) = node.borrow().right.clone() {
                self.visit(right.clone());
                self.dot_body.push(format!(
                    "    node{} -> node{}\n",
                    node.borrow()._num,
                    right.borrow()._num
                ));
            }
        }

        fn visit_var(&mut self, node: RefAST) {
            self.dot_body.push(format!(
                "    node{} [label=\"{}\"]\n",
                self.node_count,
                node.borrow().stat.get_token().get_value()
            ));
            node.borrow_mut()._num = self.node_count;
            self.node_count += 1;
        }

        fn visit_noop(&mut self, node: RefAST) {
            self.dot_body
                .push(format!("    node{} [label=\"NoOp\"]\n", self.node_count));
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

    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::fs::File;
    use std::io::{Read, Write};
    use std::process::Command;

    use super::gen_ast_dot::*;

    const PATH: &str = "./src/tests/";

    #[test]
    fn test_gen_ast_dot() {
        if let Ok(mut file) = File::open(format!("{}test.pas", PATH)) {
            let mut input = String::new();
            file.read_to_string(&mut input).expect("can't read file.");
            let tokens = Lexer::new(input).get_tokens();

            let tree = Parser::new(&tokens).parser();

            let mut output = ASTVisualizer::new()._visit(tree);
            output.push("}\n".to_string());
            let output = output.join("");

            let dot_file_name = "ast.dot";
            let dot_file_path = format!("{}{}", PATH, dot_file_name);
            let dot_file = File::create(dot_file_path);
            if let Ok(mut f) = dot_file {
                f.write_all(output.as_ref()).unwrap();
            }

            // The graphviz command client should be installed
            let mut dot = Command::new("dot");
            let _ = dot
                .args([
                    "-Tpng",
                    "-o",
                    &format!("{}ast.png", PATH),
                    &format!("{}ast.dot", PATH),
                ])
                .status();
        }
    }
}
