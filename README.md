# Inventário --- Projeto em Haskell

## Instituição

**Pontifiicia Universidade Catolica do Parana**

## Disciplina

**Ciencia Da Computacao**

## Professor

**Frank Coelho De Alcantara**

## Alunos (em ordem alfabética)

-   **Adryan Costa** --- GitHub:
    [@AdryanCostaSilva](https://github.com/AdryanCostaSilva)
-   **Hassan Ali** --- GitHub:
    [@haskaz313](https://github.com/haskaz313)
-   **Hussein Ali** --- GitHub:
    [@ItsPoyoyo](https://github.com/ItsPoyoyo)
-   **Murilo Zimerman** --- GitHub:
    [@MuriloZF](https://github.com/MuriloZF)

------------------------------------------------------------------------

## Visão geral do projeto

Este repositório reúne um sistema de inventário desenvolvido em
**Haskell**, criado para registrar itens, atualizar quantidades, listar
informações e gerar relatórios sobre a movimentação dos objetos
cadastrados.

O código foi organizado para separar a lógica pura da parte de arquivos
e interação com o usuário, deixando o fluxo do programa fácil de
entender e manter.

------------------------------------------------------------------------

## Principais funcionalidades

-   Adicionar itens ao inventário\
-   Remover quantidades existentes\
-   Listar todos os itens cadastrados\
-   Emitir relatórios completos\
-   Registrar operações em log (sucesso e erro)

------------------------------------------------------------------------

## Arquitetura do Código

A construção do sistema segue quatro componentes principais:

### 1. Estrutura Geral

-   Modelos de dados\
-   Lógica pura\
-   Persistência\
-   Interação com usuário

### 2. Modelos de Dados

-   `Item` --- ID, nome, quantidade e categoria\
-   `Inventario` --- `Map String Item`\
-   `LogEntry` --- ação, horário e status

### 3. Lógica Pura

Funções de regra e validação: - `addItem` - `removeItem` -
`deleteItem` - `updateQty`

Retorno sempre no formato:

    Either String (Inventario, LogEntry)

### 4. Persistência

-   `Inventario.dat` --- inventário salvo\
-   `Auditoria.log` --- registros das ações

Utiliza `readMaybe`, `catch` e serialização via `Show`/`Read`.

### 5. Auditoria e Relatórios

Funções: - `logsDeErro` - `logsDeSucesso` - `historicoPorItem` -
`itemMaisMovimentado`

### 6. Loop Principal

Fluxo: 1. Espera comando\
2. Coleta informações\
3. Executa função pura\
4. Salva inventário e log\
5. Retorna ao prompt

Comandos disponíveis: - `add` - `remove` - `delete` - `update` -
`listar` - `report` - `historico` - `sair`

### 7. Exemplo do fluxo de "add"

1.  Usuário digita `add`\
2.  Sistema pergunta ID, nome, quantidade, categoria\
3.  Lógica pura valida\
4.  Persistência grava\
5.  Log é atualizado

### 8. Estrutura dos Arquivos

-   `Inventario.dat`\
-   `Auditoria.log`

### 9. Características Técnicas

-   Uso de `Map`\
-   Validação com `Either`\
-   Separação total entre IO e lógica pura\
-   Auditoria com `UTCTime`

------------------------------------------------------------------------

## Como executar no OnlineGDB

1.  Abra: https://onlinegdb.com/IujVRtNv0K\
2.  Clique **Run**.

------------------------------------------------------------------------

## Exemplo de uso

### Adicionar item

    add
    ID: A1
    Nome: Teclado Mecânico
    Quantidade: 10
    Categoria: Periféricos

Saída:

    Operacao realizada com sucesso!

### Remover quantidade

    remove
    ID do item: A1
    Quantidade a remover: 3

Saída:

    Operacao realizada com sucesso!

### Listar itens

    listar

Saída:

    === Itens no Inventario ===
    ID: A1 | Nome: Teclado Mecânico | Qtd: 7 | Categoria: Periféricos

### Gerar relatório

    report

Saída:

    === Relatorio Completo ===
    Total de itens no inventario: 1
    Total de operacoes registradas: 3
    --- Itens no Inventario ---
    ID: A1 | Nome: Teclado Mecânico | Qtd: 7 | Categoria: Periféricos
    --- Logs de Sucesso ---
    Total de operacoes bem-sucedidas: 3
    --- Item Mais Movimentado ---
    Item: ID=A1 | Nome=Teclado Mecânico | Operacoes=3
