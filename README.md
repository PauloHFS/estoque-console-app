<h2>SMART MGMT: GERENCIAMENTO DE ESTOQUE RÁPIDO E SIMPLES</h2>

---


Esta é uma aplicação proposta como projeto da disciplina de Paradigmas de Linguagens de Programação. Ela será desenvolvida em duas linguagens:
- Haskell;
- Prolog.

<h3>Problemática</h3>
Em uma conveniência muito movimentada, percebeu-se um desfalque muito grande no estoque físico comparado ao das anotações do gerente, que não tinha tempo de avaliar em cada compra que produtos estavam saindo da loja.

<h3>Proposta</h3>
Informatizar as anotações do gerente, tirando do papel o gerenciamento do estoque e colocando-o em um sistema computacional, a fim de facilitar operações realizadas diversas vezes, automatizando o processo.

<h3>Objetivo</h3>

Manter o controle do estoque da conveniência, permitindo à gerência total controle dos fluxos do estoque, auxiliando nas tomadas de decisão, como, por exemplo, na compra de novos produtos que estavam em falta.

<h3>Funcionalidades</h3>

1. **Interface de usuário:**<br>
    Para acessar o console app, o usuário (gerente, nesse cenário) fará, pelo terminal, uma chamada no shell. Ao ser executado, o app exibirá, a priori, um Menu com as opções disponíveis para gerenciar seu estoque. Sendo elas:
    - **Criar produto no estoque:** O usuário irá cadastrar uma nova entrada no estoque, informando o nome, quantidade, preço e a validade restante (em meses) do produto.
    
    - **Atualizar quantidade de produtos:** O usuário informará o ID do produto e a quantidade que será subtraída do estoque, após efetuada uma compra.
 
    - **Listar produtos:** Retorna uma lista com todos os produtos atualmente disponíveis no estoque.
    
    - **Atualizar valor de produto:** O usuário informa o ID do produto e o seu novo valor (em reais).
    
    - **Deletar produto:** O usuário informa o ID do produto que será removido por completo do estoque.
    
    - **Verificar validade:** Retorna uma lista com os produtos que estão próximos do vencimento e ou vencidos (caso existam).
    
    - **Verificar itens zerados:** Retorna uma lista com os produtos com baixa quantidade no estoque.
    
2. **Estrutura dos dados:**<br>
  Neste App os dados serão salvos em formato csv, permitindo a qualquer momento sua importação em softwares de análise mais complexos.

## Para o rodar o projeto:

1. Clone o projeto;
2. Siga as instruções propostas para cada um dos paradigmas: você as encontrará em estoque-console-app/haskell/ (para o paradigma funcional), e estoque-console-app/prolog/ (para o paradigma lógico).
