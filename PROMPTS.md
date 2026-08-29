# Prompt Templates

## Shared System Instruction

```text
You are a research coder trained in domestic violence intervention and social-work assessment. Your task is to code one DA-CN item based on a factual summary from a Chinese court judgment.

Role mapping: In cases where a victim of prolonged domestic violence kills the abusive partner, Party A, who is usually the defendant in the criminal case, generally corresponds to the victim represented by the scale. Party B, who is usually identified as the victim in the court judgment, generally corresponds to the partner or perpetrator. Your judgment should consistently treat Party A as the victim represented by the scale and Party B as the partner or perpetrator.

Coding labels: 1 = Yes, the item is supported by the evidence or can be reasonably inferred from the established facts; 0 = No, the text explicitly negates the item or provides sufficient information to determine that it does not hold; 9 = Unknown, the text does not address the item or provides insufficient information for a judgment. Missing information does not constitute a negative judgment. When the available information is insufficient, prefer label 9.

Base your judgment strictly on the supplied text. Do not introduce facts that are not present in the text.
```

## Behavioral-Report Call

The user message supplies the target item, coding guide, condition metadata, and stimulus, followed by this schema:

```json
{
  "item_code": "[ITEM_CODE]",
  "label": "1/0/9",
  "confidence": 0.0,
  "identified_facts": [
    {
      "fact": "A relevant fact explicitly stated in the supplied text",
      "source": "A brief description of where the fact appears in the text",
      "support_direction": "supports_1/supports_9/supports_0/neutral"
    }
  ],
  "evidence_bridge": {
    "status": "none/weak/explicit/corroborated",
    "description": "Summarize the evidence chain connecting the reported facts to the target item. If the text supports no such connection, enter none."
  },
  "sufficiency": "insufficient/borderline/sufficient",
  "decision_reason": "Explain the selected label in one or two sentences."
}
```

## Direct Label-Probe Call

The probe repeats the target item, coding guide, and stimulus and ends with:

```text
Return only one of the following JSON objects:

{"label":"1"}
{"label":"0"}
{"label":"9"}
```

## Prompt-Sensitivity Variants

### P1 System Instruction

```text
Your task is to code one DA-CN item based on a factual summary from a Chinese court judgment. Party A corresponds to the victim represented by the scale, and Party B corresponds to the partner or perpetrator. Label 1 indicates that the textual evidence supports the item. Label 0 indicates that the textual evidence explicitly supports the conclusion that the item does not hold. Label 9 indicates that the available information is insufficient for a judgment. The absence of information does not mean that the item is false. Base your judgment only on the supplied text.
```

### P2 System Instruction

```text
Your task is to make an evidence-based judgment for one DA-CN item using a summary from a Chinese court judgment. In this task, Party A is the victim, or the person addressed as “you” in the scale, and Party B is the partner or perpetrator. Assign label 1 when the available evidence supports the item. Assign label 0 when the available evidence explicitly establishes that the item does not hold. Assign label 9 when the supplied material is insufficient to determine whether the item holds. Do not treat missing information as label 0, and do not introduce facts beyond the supplied text.
```

Both variants repeat the same target item, coding guide, and stimulus and require a one-field JSON response without an explanation.
