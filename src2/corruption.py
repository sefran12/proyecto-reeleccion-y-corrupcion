import random
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from collections import defaultdict

# Define the protection function delta(k_p, k_c)
def protection_function(k_p, k_c, theta_p, theta_c):
    return min(1, theta_p * k_p + theta_c * k_c)

class Agent:
    def __init__(self):
        self.utility = 0

class PublicAgent(Agent):
    def __init__(self, theta_p, base_salary=0):
        super().__init__()
        self.theta_p = theta_p
        self.k_p = 0
        self.base_salary = base_salary
        self.b_a_paid = 0  # Bribe paid to auditor

    def decide_protection(self, alpha, S_p, theta_c, k_c):
        """
        Decide on protection investment based on utility maximization.
        """
        # Simple decision: invest enough to set delta = 1 if beneficial
        required_delta = 1
        potential_delta = self.theta_p * 1  # Assuming k_p = 1 for simplicity
        if alpha * self.theta_p * S_p > 1 and potential_delta + self.theta_p * k_c < required_delta:
            # Invest enough to contribute to delta
            self.k_p = min(1, (required_delta - self.theta_p * k_c) / self.theta_p)
        else:
            self.k_p = 0
        return self.k_p

    def offer_bribe_auditor(self, base_bribe_a):
        """
        Decide how much to offer as bribe to the auditor.
        """
        # Simple strategy: always offer a fixed bribe if corruption is being attempted
        return base_bribe_a

    def accept_bribe(self, b, Pr_detection, S_p):
        """
        Decide whether to accept a bribe from the private agent.
        """
        expected_sanction = Pr_detection * S_p
        return (b - expected_sanction) > 0

    def compute_utility(self, b_received, Pr_detection, S_p, base_salary, b_a_paid):
        """
        Compute the utility of the public agent.
        """
        self.utility = b_received - self.k_p - Pr_detection * S_p + base_salary - b_a_paid

class PrivateAgent(Agent):
    def __init__(self, theta_c):
        super().__init__()
        self.theta_c = theta_c
        self.k_c = 0
        self.b_a_paid = 0  # Bribe paid to auditor

    def decide_protection(self, alpha, S_c, theta_p, k_p):
        """
        Decide on protection investment based on utility maximization.
        """
        # Simple decision: invest enough to set delta = 1 if beneficial
        required_delta = 1
        potential_delta = self.theta_c * 1  # Assuming k_c =1 for simplicity
        if alpha * self.theta_c * S_c > 1 and potential_delta + theta_p * k_p < required_delta:
            # Invest enough to contribute to delta
            self.k_c = min(1, (required_delta - theta_p * k_p) / self.theta_c)
        else:
            self.k_c = 0
        return self.k_c

    def offer_bribe_auditor(self, base_bribe_a):
        """
        Decide how much to offer as bribe to the auditor.
        """
        # Simple strategy: always offer a fixed bribe if corruption is being attempted
        return base_bribe_a

    def offer_bribe(self, p, c, b):
        """
        Decide whether to offer a bribe to the public agent.
        Returns True if offering, False otherwise.
        """
        return (p - c - b) > 0

    def compute_utility(self, p, c, b_paid, Pr_detection, S_c, b_a_paid):
        """
        Compute the utility of the private agent.
        """
        self.utility = (p - c) - b_paid - self.k_c - Pr_detection * S_c - b_a_paid

class Auditor(Agent):
    def __init__(self):
        super().__init__()
        self.k_a = 0
        self.b_a_received = 0  # Total bribe received

    def decide_audit(self, Pr_detection, R_a, k_a, b_a):
        """
        Decide whether to audit based on expected utility.
        If a bribe is offered and accepted, do not audit.
        """
        expected_utility_audit = -k_a + Pr_detection * R_a
        if b_a > expected_utility_audit:
            # Accept the bribe, do not audit
            return False, b_a
        else:
            # Decide to audit or not based on expected utility
            if expected_utility_audit > 0:
                return True, 0
            else:
                return False, 0

    def compute_utility(self, audited, detected, R_a, k_a, b_a):
        """
        Compute the utility of the auditor.
        """
        if audited:
            self.utility = -self.k_a
            if detected:
                self.utility += R_a
        else:
            self.utility = b_a

def simulate_project_period(public_agent, private_agent, auditor, project_params):
    """
    Simulate a single project lifecycle within a single period.
    """
    # Unpack project parameters
    V = project_params['project_value']
    c = project_params['construction_cost']
    b = project_params['bribe_p_c']
    b_a_p = project_params['bribe_a_p']
    b_a_c = project_params['bribe_a_c']
    k_a = project_params['audit_cost']
    S_p = project_params['sanction_p']
    S_c = project_params['sanction_c']
    R_a = project_params['reward_a']
    alpha = project_params['alpha']
    theta_p = project_params['theta_p']
    theta_c = project_params['theta_c']
    base_salary = project_params['base_salary']

    # Initialize Auditor's cost
    auditor.k_a = k_a

    # Step 1: Public and Private Agents decide on protection
    public_agent.decide_protection(alpha, S_p, theta_c, k_c=private_agent.k_c)
    private_agent.decide_protection(alpha, S_c, theta_p, k_p=public_agent.k_p)

    # Compute overall detection probability
    delta = protection_function(public_agent.k_p, private_agent.k_c, theta_p, theta_c)
    Pr_detection = alpha * (1 - delta)

    # Step 2: Private Agent decides to offer bribe to Public Agent
    offers_bribe = private_agent.offer_bribe(p=V, c=c, b=b)

    # Initialize variables
    corruption = False
    b_received = 0
    b_paid = 0
    b_a_total = 0

    if offers_bribe:
        accepts_bribe = public_agent.accept_bribe(b, Pr_detection, S_p)
        if accepts_bribe:
            corruption = True
            b_received = b
            b_paid = b
            b_a_p_offered = public_agent.offer_bribe_auditor(project_params['base_bribe_a_p'])
            b_a_c_offered = private_agent.offer_bribe_auditor(project_params['base_bribe_a_c'])
            b_a_total = b_a_p_offered + b_a_c_offered
            auditor.b_a_received = b_a_total

    # Step 3: Auditor decides whether to audit or accept the bribe
    audited = False
    detected = False
    if corruption:
        audited, b_a = auditor.decide_audit(Pr_detection, R_a, k_a, auditor.b_a_received)
    else:
        audited, b_a = False, 0

    # Step 4: Determine outcomes
    if audited and corruption:
        detected = random.random() < Pr_detection
        if detected:
            public_agent.compute_utility(b_received, Pr_detection, S_p, base_salary, b_a_paid=0)
            private_agent.compute_utility(p=V, c=c, b_paid=b_paid, Pr_detection=Pr_detection, S_c=S_c, b_a_paid=0)
            auditor.compute_utility(audited=True, detected=True, R_a=R_a, k_a=k_a, b_a=0)
        else:
            public_agent.compute_utility(b_received, Pr_detection, S_p, base_salary, b_a_paid=0)
            private_agent.compute_utility(p=V, c=c, b_paid=b_paid, Pr_detection=Pr_detection, S_c=S_c, b_a_paid=0)
            auditor.compute_utility(audited=True, detected=False, R_a=R_a, k_a=k_a, b_a=0)
    elif corruption and not audited:
        public_agent.compute_utility(b_received, Pr_detection=0, S_p=0, base_salary=base_salary, b_a_paid=0)
        private_agent.compute_utility(p=V, c=c, b_paid=b_paid, Pr_detection=0, S_c=0, b_a_paid=0)
        auditor.compute_utility(audited=False, detected=False, R_a=0, k_a=k_a, b_a=b_a_total)
    else:
        public_agent.utility = public_agent.base_salary - public_agent.k_p
        private_agent.utility = V - c - private_agent.k_c
        auditor.compute_utility(audited=False, detected=False, R_a=0, k_a=k_a, b_a=0)

    return {
        'corruption': corruption,
        'detected': detected if corruption and audited else False,
        'audited': audited,
        'U_P': public_agent.utility,
        'U_C': private_agent.utility,
        'U_A': auditor.utility,
        'project_value': V,
        'construction_cost': c,
        'bribe': b,
        'protection_p': public_agent.k_p,
        'protection_c': private_agent.k_c,
        'detection_prob': Pr_detection
    }

def run_simulation_with_analytics(n_periods, num_projects_per_period):
    """
    Run simulation with detailed analytics collection
    """
    results_data = []
    period_stats = defaultdict(list)

    for period in range(1, n_periods + 1):
        period_corrupt_projects = 0
        period_detected_corruptions = 0
        period_audits = 0
        
        for _ in range(num_projects_per_period):
            # Generate project parameters
            project_value = random.uniform(100, 1000)
            construction_cost = random.uniform(50, project_value * 0.8)
            b = random.uniform(10, 100)
            base_bribe_a_p = random.uniform(5, 50)
            base_bribe_a_c = random.uniform(5, 50)
            k_a = random.uniform(5, 50)
            S_p = random.uniform(100, 500)
            S_c = random.uniform(100, 500)
            R_a = random.uniform(50, 200)
            alpha = random.uniform(0.1, 0.9)
            theta_p = random.uniform(0.1, 1.0)
            theta_c = random.uniform(0.1, 1.0)
            base_salary = random.uniform(50, 200)

            project_params = {
                'project_value': project_value,
                'construction_cost': construction_cost,
                'bribe_p_c': b,
                'bribe_a_p': base_bribe_a_p,
                'bribe_a_c': base_bribe_a_c,
                'audit_cost': k_a,
                'sanction_p': S_p,
                'sanction_c': S_c,
                'reward_a': R_a,
                'alpha': alpha,
                'theta_p': theta_p,
                'theta_c': theta_c,
                'base_salary': base_salary,
                'base_bribe_a_p': base_bribe_a_p,
                'base_bribe_a_c': base_bribe_a_c
            }

            # Initialize agents
            public_agent = PublicAgent(theta_p=theta_p, base_salary=base_salary)
            private_agent = PrivateAgent(theta_c=theta_c)
            auditor = Auditor()

            # Simulate project
            result = simulate_project_period(public_agent, private_agent, auditor, project_params)
            
            # Add period information
            result['period'] = period
            results_data.append(result)

            # Update period statistics
            if result['corruption']:
                period_corrupt_projects += 1
            if result['detected']:
                period_detected_corruptions += 1
            if result['audited']:
                period_audits += 1

        # Store period statistics
        period_stats['corruption_rate'].append(period_corrupt_projects / num_projects_per_period)
        period_stats['detection_rate'].append(
            period_detected_corruptions / period_corrupt_projects if period_corrupt_projects > 0 else 0
        )
        period_stats['audit_rate'].append(period_audits / num_projects_per_period)

    return results_data, period_stats

def analyze_and_plot_results(results_data, period_stats):
    """
    Analyze results and create visualizations
    """
    # Convert results to DataFrame
    df = pd.DataFrame(results_data)
    
    # 1. Corruption Rate Over Time
    plt.figure(figsize=(12, 6))
    plt.plot(range(1, len(period_stats['corruption_rate']) + 1), 
            period_stats['corruption_rate'], 
            marker='o')
    plt.title('Corruption Rate Over Time')
    plt.xlabel('Period')
    plt.ylabel('Corruption Rate')
    plt.grid(True)
    plt.show()

    # 2. Utilities Distribution
    plt.figure(figsize=(12, 6))
    plt.boxplot([df['U_P'], df['U_C'], df['U_A']], 
                tick_labels=['Public Agent', 'Private Agent', 'Auditor'])
    plt.title('Distribution of Utilities')
    plt.ylabel('Utility')
    plt.grid(True)
    plt.show()

    # 3. Protection Levels vs Detection Probability
    plt.figure(figsize=(12, 6))
    plt.scatter(df['protection_p'] + df['protection_c'], 
                df['detection_prob'], 
                alpha=0.5)
    plt.title('Total Protection vs Detection Probability')
    plt.xlabel('Total Protection (Public + Private)')
    plt.ylabel('Detection Probability')
    plt.grid(True)
    plt.show()

    # 4. Project Value vs Corruption
    plt.figure(figsize=(12, 6))
    plt.hist([df[df['corruption']]['project_value'], 
              df[~df['corruption']]['project_value']], 
             label=['Corrupt', 'Non-corrupt'], 
             bins=30, 
             alpha=0.7)
    plt.title('Project Value Distribution: Corrupt vs Non-corrupt Projects')
    plt.xlabel('Project Value')
    plt.ylabel('Frequency')
    plt.legend()
    plt.grid(True)
    plt.show()

    # Calculate and print summary statistics
    print("\n=== Summary Statistics ===")
    print(f"Overall Corruption Rate: {df['corruption'].mean():.2%}")
    print(f"Average Detection Rate: {df[df['corruption']]['detected'].mean():.2%}")
    print(f"Average Audit Rate: {df['audited'].mean():.2%}")
    
    print("\nUtility Statistics:")
    utility_stats = df[['U_P', 'U_C', 'U_A']].agg(['mean', 'std', 'min', 'max'])
    utility_stats.columns = ['Public Agent', 'Private Agent', 'Auditor']
    print(utility_stats)

    # Correlation analysis
    print("\nCorrelation Analysis:")
    corr_matrix = df[['project_value', 'protection_p', 'protection_c', 
                      'detection_prob', 'U_P', 'U_C', 'U_A']].corr()
    print(corr_matrix)

if __name__ == "__main__":
    # Simulation parameters
    n_periods = 10
    num_projects_per_period = 100

    # Run simulation with analytics
    results_data, period_stats = run_simulation_with_analytics(n_periods, num_projects_per_period)
    
    # Analyze and visualize results
    analyze_and_plot_results(results_data, period_stats)
