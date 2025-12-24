#!/usr/bin/env python3
"""
Task Planner - PERT estimation and agent assignment for scheme-llm-toolkit

Usage:
    python scripts/task-planner.py                    # Show all tasks
    python scripts/task-planner.py --agent claude    # Tasks for Claude
    python scripts/task-planner.py --phase 1         # Phase 1 tasks
    python scripts/task-planner.py --task 013-gemini # Task details
    python scripts/task-planner.py --portfolio       # Portfolio analysis
"""

import yaml
import argparse
import math
from pathlib import Path


def load_config():
    """Load the task configuration."""
    config_path = Path(__file__).parent.parent / "AGENT_TASK_CONFIG.yaml"
    with open(config_path) as f:
        return yaml.safe_load(f)


def pert_expected(o, l, p):
    """Calculate PERT expected value: (O + 4L + P) / 6"""
    return (o + 4 * l + p) / 6


def pert_stddev(o, p):
    """Calculate PERT standard deviation: (P - O) / 6"""
    return (p - o) / 6


def calculate_task_metrics(task):
    """Calculate derived metrics for a task."""
    effort = task.get("effort", {})
    o = effort.get("optimistic", 1)
    l = effort.get("likely", 2)
    p = effort.get("pessimistic", 4)

    expected = pert_expected(o, l, p)
    stddev = pert_stddev(o, p)

    # 95% confidence interval (mean ± 2*stddev)
    ci_low = max(o, expected - 2 * stddev)
    ci_high = expected + 2 * stddev

    # Risk score (0-1): higher = riskier
    risk_factors = task.get("risk_factors", [])
    risk_score = sum(
        rf.get("probability", 0.5) * ({"low": 0.2, "medium": 0.5, "high": 1.0}.get(rf.get("impact", "medium"), 0.5))
        for rf in risk_factors
    ) / max(len(risk_factors), 1)

    # Cost
    cost = task.get("cost_estimate", {})
    estimated_usd = cost.get("estimated_usd", 0)

    return {
        "expected_effort": round(expected, 1),
        "stddev": round(stddev, 2),
        "ci_95": (round(ci_low, 1), round(ci_high, 1)),
        "risk_score": round(risk_score, 2),
        "estimated_usd": estimated_usd
    }


def get_best_agent(task):
    """Determine the best agent for a task."""
    confidences = task.get("agent_confidence", {})
    if not confidences:
        return "any", 0.7

    best = max(confidences.items(), key=lambda x: x[1])
    return best[0], best[1]


def print_task_summary(task, metrics):
    """Print a formatted task summary."""
    best_agent, confidence = get_best_agent(task)

    print(f"\n{'='*60}")
    print(f"Task: {task['id']} - {task['name']}")
    print(f"{'='*60}")
    print(f"Category: {task.get('category', 'unknown')}")
    print(f"Priority: {task.get('priority', 'P3')}")
    print()
    print("PERT Estimation:")
    print(f"  Optimistic:   {task.get('effort', {}).get('optimistic', '?')} SP")
    print(f"  Likely:       {task.get('effort', {}).get('likely', '?')} SP")
    print(f"  Pessimistic:  {task.get('effort', {}).get('pessimistic', '?')} SP")
    print(f"  Expected:     {metrics['expected_effort']} SP (σ={metrics['stddev']})")
    print(f"  95% CI:       [{metrics['ci_95'][0]}, {metrics['ci_95'][1]}] SP")
    print()
    print("Agent Confidence:")
    for agent, conf in task.get("agent_confidence", {}).items():
        bar = "█" * int(conf * 20) + "░" * (20 - int(conf * 20))
        marker = " ← best" if agent == best_agent else ""
        print(f"  {agent:12} [{bar}] {conf:.0%}{marker}")
    print()
    print(f"Risk Score: {metrics['risk_score']:.0%}")
    if task.get("risk_factors"):
        for rf in task["risk_factors"]:
            print(f"  - [{rf.get('probability', 0.5):.0%} prob, {rf.get('impact', 'medium')} impact] {rf.get('description', '')}")
    print()
    print(f"Estimated Cost: ${metrics['estimated_usd']:.2f}")
    print()
    print("Recommendation:")
    if confidence >= 0.8:
        print(f"  ✓ Assign to {best_agent} (high confidence)")
    elif confidence >= 0.6:
        print(f"  ~ Assign to {best_agent} with review checkpoints")
    else:
        print(f"  ! Consider research spike or human support")


def print_portfolio_analysis(config):
    """Analyze the full task portfolio."""
    tasks = config.get("tasks", [])

    print("\n" + "="*60)
    print("PORTFOLIO ANALYSIS")
    print("="*60)

    total_expected = 0
    total_cost = 0
    by_phase = {}

    for task in tasks:
        metrics = calculate_task_metrics(task)
        total_expected += metrics["expected_effort"]
        total_cost += metrics["estimated_usd"]

    print(f"\nTotal Tasks: {len(tasks)}")
    print(f"Total Expected Effort: {total_expected:.0f} story points")
    print(f"Total Estimated Cost: ${total_cost:.2f}")

    # By category
    print("\nBy Category:")
    categories = {}
    for task in tasks:
        cat = task.get("category", "other")
        if cat not in categories:
            categories[cat] = {"count": 0, "effort": 0, "cost": 0}
        metrics = calculate_task_metrics(task)
        categories[cat]["count"] += 1
        categories[cat]["effort"] += metrics["expected_effort"]
        categories[cat]["cost"] += metrics["estimated_usd"]

    for cat, data in sorted(categories.items()):
        print(f"  {cat:15} {data['count']:2} tasks, {data['effort']:5.1f} SP, ${data['cost']:.2f}")

    # Execution phases
    print("\nExecution Strategy:")
    strategy = config.get("execution_strategy", {})
    for phase_key, phase in strategy.items():
        print(f"\n  {phase_key}:")
        print(f"    {phase.get('description', '')}")
        print(f"    Tasks: {', '.join(phase.get('tasks', []))}")
        print(f"    Effort: {phase.get('total_expected_effort', '?')} SP")
        print(f"    Confidence: {phase.get('combined_confidence', 0):.0%}")


def print_agent_tasks(config, agent):
    """Print tasks recommended for a specific agent."""
    tasks = config.get("tasks", [])
    agent = agent.lower()

    print(f"\n{'='*60}")
    print(f"TASKS FOR: {agent.upper()}")
    print(f"{'='*60}")

    high_conf = []
    medium_conf = []
    low_conf = []

    for task in tasks:
        conf = task.get("agent_confidence", {}).get(agent, 0.5)
        metrics = calculate_task_metrics(task)
        entry = (task["id"], task["name"], conf, metrics["expected_effort"], metrics["risk_score"])

        if conf >= 0.8:
            high_conf.append(entry)
        elif conf >= 0.6:
            medium_conf.append(entry)
        else:
            low_conf.append(entry)

    def print_group(name, items):
        if items:
            print(f"\n{name}:")
            for id, name, conf, effort, risk in sorted(items, key=lambda x: -x[2]):
                risk_indicator = "⚠" if risk > 0.4 else " "
                print(f"  {risk_indicator} {id:20} {conf:.0%} conf, {effort:.1f} SP - {name}")

    print_group("High Confidence (≥80%)", high_conf)
    print_group("Medium Confidence (60-80%)", medium_conf)
    print_group("Low Confidence (<60%) - Consider alternatives", low_conf)


def main():
    parser = argparse.ArgumentParser(description="Task Planner for scheme-llm-toolkit")
    parser.add_argument("--agent", help="Show tasks for specific agent (claude, gemini, gpt4, local)")
    parser.add_argument("--task", help="Show details for specific task ID")
    parser.add_argument("--phase", type=int, help="Show tasks for execution phase (1-4)")
    parser.add_argument("--portfolio", action="store_true", help="Show portfolio analysis")
    parser.add_argument("--list", action="store_true", help="List all tasks briefly")

    args = parser.parse_args()
    config = load_config()

    if args.task:
        tasks = {t["id"]: t for t in config.get("tasks", [])}
        if args.task in tasks:
            task = tasks[args.task]
            metrics = calculate_task_metrics(task)
            print_task_summary(task, metrics)
        else:
            print(f"Task not found: {args.task}")
            print(f"Available: {', '.join(tasks.keys())}")

    elif args.agent:
        print_agent_tasks(config, args.agent)

    elif args.portfolio:
        print_portfolio_analysis(config)

    elif args.list:
        print("\n" + "="*70)
        print(f"{'ID':<20} {'Name':<30} {'Effort':<8} {'Best Agent'}")
        print("="*70)
        for task in config.get("tasks", []):
            metrics = calculate_task_metrics(task)
            best_agent, conf = get_best_agent(task)
            print(f"{task['id']:<20} {task['name'][:28]:<30} {metrics['expected_effort']:<8.1f} {best_agent} ({conf:.0%})")

    else:
        # Default: show summary
        print_portfolio_analysis(config)
        print("\nUse --help for more options")


if __name__ == "__main__":
    main()
