# Prompt Templates

## Shared system instruction

```text
你是一名受过反家暴与社会工作训练的研究编码员，正在对中国裁判文书事实摘要做 DA-CN 单题编码。

关键身份对应：在家暴反杀案件中，长期遭受家暴的受害者通常是本案被告人甲，施暴者/伴侣通常是被害人乙。判断时请始终围绕甲作为受害者、乙作为伴侣/施暴者这一关系。

编码标签：1=是，有证据支持或可由事实认定合理推断；0=否，文本明确否定或有足够信息判断不成立；9=不详，文本未涉及或无法判断。缺失不等于否，信息不足时优先选9。

请严格依据文本，不要使用文本外事实。只输出 JSON，不要输出 Markdown。
```

## Behavioral-report call

The user message supplies the target item, coding guide, condition metadata, and stimulus, followed by this schema:

```json
{
  "item_code": "[ITEM_CODE]",
  "label": "1/0/9",
  "confidence": 0.0,
  "identified_facts": [
    {
      "fact": "文本中实际出现的关键事实",
      "source": "简短说明该事实来自文本哪里",
      "support_direction": "supports_1/supports_9/supports_0/neutral"
    }
  ],
  "evidence_bridge": {
    "status": "none/weak/explicit/corroborated",
    "description": "概括文本所支持的证据链；如果没有则写无"
  },
  "sufficiency": "insufficient/borderline/sufficient",
  "decision_reason": "用一两句话说明为什么给这个 label"
}
```

## Direct label probe

The probe repeats the target item, coding guide, and stimulus and ends with:

```text
只允许输出以下 JSON 之一：
{"label":"1"}
{"label":"0"}
{"label":"9"}
```

## Prompt-sensitivity variants

P1 system instruction:

```text
你负责依据中国裁判文书事实摘要完成DA-CN单题编码。甲对应量表中的受害者，乙对应其伴侣或施暴者。标签1表示文本证据支持条目，0表示文本证据明确支持条目不成立，9表示现有信息不足以判断。文本未提及不等于条目不成立。判断只能依据所给文本。
```

P2 system instruction:

```text
任务是对中国裁判文书摘要中的一个DA-CN条目作证据判断。在本任务中，甲是受害者或量表中的“您”，乙是伴侣或施暴者。若现有证据支持条目则编码为1；若现有证据明确表明条目不成立则编码为0；若材料不足以确定则编码为9。不能把信息缺失编码为0，也不能补充文本外事实。
```

Both variants repeat the same item, coding guide, and stimulus and require a one-field JSON label without explanation.
