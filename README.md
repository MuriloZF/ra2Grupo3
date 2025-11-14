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

Esta seção descreve todos os modelos utilizados pelo sistema, seus campos, invariantes, estruturas e exemplos. Esses modelos representam o domínio do inventário e o sistema de auditoria, garantindo persistência, consistência e rastreamento de todas as operações realizadas.

---

## `Item`
Representa um produto cadastrado no inventário.

- **Campos**
  - `itemID :: String` — Identificador único do item.  
  - `nome :: String` — Nome legível do item.  
  - `quantidade :: Int` — Quantidade disponível em estoque (>= 0).  
  - `categoria :: String` — Categoria à qual o item pertence.  

- **Invariantes**
  - `itemID` deve ser único no inventário.  
  - `quantidade` nunca pode ser negativa.  
  - `nome` e `categoria` devem ser válidos (não vazios).  

- **Exemplo**
  ```haskell
  Item { itemID = "A01", nome = "Teclado", quantidade = 10, categoria = "Periféricos" }
- O módulo Inventario é responsável por armazenar e gerenciar todos os itens registrados no sistema, fornecendo operações de CRUD com auditoria completa.

- Estruturas de Dados
- Inventario
- haskell
- type Inventario = Map String Item
- Propriedades: Cada chave do mapa corresponde ao itemID

- Persistência: Armazenado em Inventario.dat usando serialização textual (show/read)

- AcaoLog
- Enum que representa o tipo da ação auditada:

- Valor	Descrição
- Add	Inserção de item
- Remove	Remoção de quantidade
- Update	Alteração de quantidade
- QueryFail	Erro de operação
- StatusLog
- Indica o resultado da operação:

- Sucesso

- Falha String — Inclui mensagem explicando o motivo da falha

- LogEntry
- Representa uma linha de registro no arquivo de auditoria:

- haskell
- LogEntry {
    - timestamp :: UTCTime,    -- Momento da operação
    - acao :: AcaoLog,         -- Tipo da ação realizada
    - detalhes :: String,      -- Descrição da operação
    - status :: StatusLog      -- Sucesso ou falha
}
- ResultadoOperacao
- Retorno padrão de funções que alteram o estado:

- haskell
- type ResultadoOperacao = (Inventario, LogEntry)
- Comportamento do Sistema
- Persistência
- Inventário: Carregado automaticamente ao iniciar o programa

- Auditoria: Atualizado e gravado a cada operação modificadora

- Arquivos:

- Inventario.dat - Estado do inventário

- Auditoria.log - Registros de auditoria em formato textual

- Operações Suportadas
- Busca de itens

- Inserção de novos itens

- Remoção de itens/quantidades

- Atualização de informações

- Regras de Validação
- Adicionar Item
- Falha se o item já existir

- Quantidade inicial deve ser >= 0

- Remover Quantidade
- Item deve existir

- Não pode remover mais do que existe

- Atualizar Quantidade
- Nunca pode resultar em número negativo

- Deletar Item
- Item deve existir

- Comportamento em Caso de Falha
- Quando uma operação falha:

- Inventário permanece inalterado

- LogEntry com status Falha é registrado

- Exemplo de Log
- haskell
- LogEntry {
  -  timestamp = 2025-11-14 18:00:00 UTC,
   - acao = Remove,
   -  detalhes = "Tentativa de remover 15 unidades do item T01",
- status = Falha "Estoque insuficiente"
}
- Implementação
- Operações via Data.Map
- O sistema utiliza Data.Map para operações eficientes de:

- Busca por itemID

- Inserção e remoção

- Atualização de valores

- Serialização
- Entrada/Saída: Serialização textual usando show e read

-Formato: Textual legível para fácil debug e manutenção


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

### Cenario 1 : 
![WhatsApp Image 2025-11-14 at 19 31 06_c08aef2f](https://github.com/user-attachments/assets/8c8ff5bb-3a23-4584-8449-1420e0dfa515)

### Cenario 2: 
![WhatsApp Image 2025-11-14 at 19 40 15_8358878e](https://github.com/user-attachments/assets/6abedc7f-8b05-4613-ade3-67ede5bb0662)
![WhatsApp Image 2025-11-14 at 19 42 22_72a1a1c0](https://github.com/user-attachments/assets/f802e20a-7483-4bda-9695-21d8024b966b)
![WhatsApp Image 2025-11-14 at 19 43 25_e7bd3c71](https://github.com/user-attachments/assets/a8bfb59f-3bce-4da9-aa38-ce46a9e86887)
![WhatsApp Image 2025-11-14 at 19 44 16_bc8cfd50](https://github.com/user-attachments/assets/c7d26db1-26a9-4a26-969c-cf6011a332cb)

### Cenario 3: 
![WhatsApp Image 2025-11-14 at 19 53 48_b57e6682](https://github.com/user-attachments/assets/6116592f-4b40-4af3-9d90-8c7479901a51)





