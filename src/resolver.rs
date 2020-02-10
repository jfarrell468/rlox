use crate::ast::{AstPrinter, Expression, MutatingVisitor, Statement};
use crate::token::Token;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ResolverError {
    message: String,
    token: Option<Token>,
}

impl<'a> fmt::Display for ResolverError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token {
            None => write!(f, "Resolver error: {}", self.message.as_str()),
            Some(token) => write!(
                f,
                "[line {}] Resolver error: {}\n  Context: {}",
                token.line, self.message, token.lexeme
            ),
        }
    }
}

impl<'a> Error for ResolverError {
    fn description(&self) -> &str {
        &self.message
    }
}

pub struct Resolver {
    scopes: Vec<BTreeMap<String, bool>>,
}

impl MutatingVisitor<Expression, Result<(), ResolverError>> for Resolver {
    fn visit(&mut self, expr: &mut Expression) -> Result<(), ResolverError> {
        match expr {
            Expression::Binary {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expression::Grouping(expr) => self.resolve_expr(expr),
            Expression::Literal(_) => Ok(()),
            Expression::Logical {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            Expression::Unary { operator: _, right } => self.resolve_expr(right),
            Expression::Variable { name, scope: _ } => {
                if let Some(x) = self.scopes.last() {
                    if !x.get(&name.lexeme).cloned().unwrap_or(true) {
                        return Err(ResolverError {
                            message: "Cannot read local variable in its own initializer."
                                .to_string(),
                            token: Some(name.clone()),
                        });
                    }
                }
                self.resolve_local(expr)
            }
            Expression::Assign {
                name: _,
                value,
                scope: _,
            } => {
                self.resolve_expr(value)?;
                self.resolve_local(expr)
            }
            Expression::Call {
                callee,
                paren: _,
                arguments,
            } => {
                self.resolve_expr(callee)?;
                for argument in arguments {
                    self.resolve_expr(argument)?
                }
                Ok(())
            }
        }
    }
}

impl MutatingVisitor<Statement, Result<(), ResolverError>> for Resolver {
    fn visit(&mut self, stmt: &mut Statement) -> Result<(), ResolverError> {
        match stmt {
            Statement::Print(expr) => self.resolve_expr(expr),
            Statement::Expression(expr) => self.resolve_expr(expr),
            Statement::Var { name, initializer } => {
                self.declare(name.clone());
                self.resolve_expr(initializer)?;
                self.define(name.clone());
                Ok(())
            }
            Statement::Block(stmts) => {
                self.begin_scope();
                let result = self.resolve(stmts);
                self.end_scope();
                result
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(some_else) = else_branch {
                    self.resolve_stmt(some_else)?
                }
                Ok(())
            }
            Statement::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)
            }
            Statement::Function(fun) => {
                self.declare(fun.name().clone());
                self.define(fun.name().clone());
                self.resolve_function(stmt)
            }
            Statement::Return { keyword: _, value } => self.resolve_expr(value),
        }
    }
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver { scopes: Vec::new() }
    }
    fn resolve_expr(&mut self, expr: &mut Expression) -> Result<(), ResolverError> {
        expr.accept_mut(self)
    }
    pub fn resolve_stmt(&mut self, stmt: &mut Statement) -> Result<(), ResolverError> {
        stmt.accept_mut(self)
    }
    pub fn resolve(&mut self, statements: &mut Vec<Statement>) -> Result<(), ResolverError> {
        for stmt in statements {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }
    fn begin_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
    fn declare(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, false);
        }
    }
    fn define(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }
    fn resolve_local(&mut self, expr: &mut Expression) -> Result<(), ResolverError> {
        let token = match expr {
            Expression::Variable { name, scope: _ } => name,
            Expression::Assign {
                name,
                value: _,
                scope: _,
            } => name,
            _ => {
                return Err(ResolverError {
                    message: "Can only resolve an expression of type 'variable' or 'assign'."
                        .to_string(),
                    token: None,
                });
            }
        };
        for (i, cur_scope) in self.scopes.iter().enumerate().rev() {
            if cur_scope.contains_key(&token.lexeme) {
                match expr {
                    Expression::Variable { name: _, scope } => {
                        *scope = Some(self.scopes.len() - 1 - i);
                        return Ok(());
                    }
                    Expression::Assign {
                        name: _,
                        value: _,
                        scope,
                    } => {
                        *scope = Some(self.scopes.len() - 1 - i);
                        return Ok(());
                    }
                    _ => {
                        return Err(ResolverError {
                            message:
                                "Can only resolve an expression of type 'variable' or 'assign'."
                                    .to_string(),
                            token: None,
                        });
                    }
                }
            }
        }
        Ok(())
    }
    fn resolve_function(&mut self, stmt: &mut Statement) -> Result<(), ResolverError> {
        match stmt {
            Statement::Function(x) => {
                self.begin_scope();
                for param in x.params().iter() {
                    self.declare(param.lexeme.clone());
                    self.define(param.lexeme.clone());
                }
                let result = self.resolve_stmt(&mut *x.body_mut());
                self.end_scope();
                result
            }
            _ => Err(ResolverError {
                message: "Can only resolve statements of type 'function.'".to_string(),
                token: None,
            }),
        }
    }
}
