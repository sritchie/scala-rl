---
layout: docs
title:  "Policies"
section: "policies"
position: 3
---

{% include_relative policies/policies.md %}

## Index

{% for x in site.pages %}
{% if x.section == 'policies' %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
{% endif %}
{% endfor %}
