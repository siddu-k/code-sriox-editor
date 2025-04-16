
import { Link } from "react-router-dom";
import { ArrowRight, Code, Download, Globe, Terminal, Zap } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardTitle } from "@/components/ui/card";

const FeatureCard = ({ icon: Icon, title, description }: { icon: any, title: string, description: string }) => (
  <Card className="bg-secondary text-secondary-foreground border-none hover:shadow-lg transition-all duration-300 hover:-translate-y-1">
    <CardContent className="p-6">
      <div className="mb-4 flex h-12 w-12 items-center justify-center rounded-full bg-primary/10">
        <Icon className="h-6 w-6 text-primary" />
      </div>
      <CardTitle className="mb-2 text-xl">{title}</CardTitle>
      <CardDescription className="text-sm text-secondary-foreground/80">{description}</CardDescription>
    </CardContent>
  </Card>
);

const LanguageCard = ({ name, color }: { name: string, color: string }) => (
  <div className={`rounded-md px-3 py-1 text-sm font-medium ${color} bg-secondary hover:bg-secondary/80 transition-colors`}>
    {name}
  </div>
);

const Index = () => {
  return (
    <div className="min-h-screen bg-background text-foreground">
      {/* Hero Section */}
      <section className="relative overflow-hidden py-20 md:py-32">
        <div className="absolute inset-0 bg-[radial-gradient(ellipse_at_center,_var(--tw-gradient-stops))] from-accent/20 via-background to-background"></div>
        <div className="container relative mx-auto px-4 z-10">
          <div className="flex flex-col items-center text-center">
            <div className="inline-block rounded-full bg-accent/10 px-3 py-1 text-sm font-medium text-primary mb-6">
              Write, Run, Share Code
            </div>
            <h1 className="text-4xl md:text-6xl font-bold mb-6 bg-clip-text text-transparent bg-gradient-to-r from-primary to-primary/70">
              codeer<span className="text-primary/90">.org</span>
            </h1>
            <p className="max-w-2xl text-xl text-muted-foreground mb-10">
              The modern code compiler platform for developers to write, test and run code in
              multiple languages directly in your browser.
            </p>
            <Link to="/compiler">
              <Button size="lg" className="group">
                Open Code Editor
                <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
              </Button>
            </Link>
          </div>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-20 bg-background">
        <div className="container mx-auto px-4">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold mb-4">Powerful Features</h2>
            <p className="text-muted-foreground max-w-2xl mx-auto">
              codeer.org provides a modern development environment with everything you need
            </p>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            <FeatureCard
              icon={Terminal}
              title="Multiple Languages"
              description="Support for 20+ programming languages including Python, JavaScript, Java, C++, and more."
            />
            <FeatureCard
              icon={Zap}
              title="Instant Execution"
              description="Run your code instantly with our powerful backend and get results in real-time."
            />
            <FeatureCard
              icon={Code}
              title="Syntax Highlighting"
              description="Beautiful syntax highlighting makes your code readable and easier to understand."
            />
            <FeatureCard
              icon={Download}
              title="Code Download"
              description="Download your code with a single click to continue your work offline."
            />
            <FeatureCard
              icon={Globe}
              title="Cross-Platform"
              description="Write and run code from any device with a modern web browser, no installation needed."
            />
            <FeatureCard
              icon={Terminal}
              title="Error Handling"
              description="Get detailed error messages and output to help you debug your code quickly."
            />
          </div>
        </div>
      </section>

      {/* Languages Section */}
      <section className="py-20 bg-accent/5">
        <div className="container mx-auto px-4">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold mb-4">Supported Languages</h2>
            <p className="text-muted-foreground max-w-2xl mx-auto">
              codeer.org supports a wide range of programming languages
            </p>
          </div>
          <div className="flex flex-wrap justify-center gap-3 max-w-4xl mx-auto">
            <LanguageCard name="Python" color="text-blue-500" />
            <LanguageCard name="JavaScript" color="text-yellow-500" />
            <LanguageCard name="C++" color="text-purple-500" />
            <LanguageCard name="Java" color="text-orange-500" />
            <LanguageCard name="C" color="text-blue-400" />
            <LanguageCard name="Go" color="text-cyan-500" />
            <LanguageCard name="Kotlin" color="text-purple-600" />
            <LanguageCard name="Ruby" color="text-red-500" />
            <LanguageCard name="TypeScript" color="text-blue-600" />
            <LanguageCard name="SQL" color="text-yellow-600" />
            <LanguageCard name="C#" color="text-green-500" />
            <LanguageCard name="PHP" color="text-indigo-500" />
            <LanguageCard name="Rust" color="text-orange-600" />
            <LanguageCard name="Swift" color="text-pink-500" />
            <LanguageCard name="Dart" color="text-blue-500" />
            <LanguageCard name="Pascal" color="text-yellow-500" />
            <LanguageCard name="Scala" color="text-red-600" />
            <LanguageCard name="Haskell" color="text-purple-500" />
            <LanguageCard name="Perl" color="text-blue-700" />
            <LanguageCard name="Clojure" color="text-green-600" />
          </div>
          <div className="mt-12 text-center">
            <Link to="/compiler">
              <Button variant="outline" size="lg" className="group">
                Start Coding Now
                <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
              </Button>
            </Link>
          </div>
        </div>
      </section>

      {/* Call to Action */}
      <section className="py-20 bg-gradient-to-br from-secondary to-secondary/70">
        <div className="container mx-auto px-4 text-center">
          <h2 className="text-3xl md:text-4xl font-bold mb-6 text-secondary-foreground">
            Ready to start coding?
          </h2>
          <p className="text-xl text-secondary-foreground/80 mb-10 max-w-2xl mx-auto">
            Join codeer.org today and experience the most powerful online code editor
          </p>
          <Link to="/compiler">
            <Button size="lg" variant="default" className="group">
              Open Code Editor
              <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
            </Button>
          </Link>
        </div>
      </section>

      {/* Footer */}
      <footer className="py-10 bg-background border-t border-border">
        <div className="container mx-auto px-4">
          <div className="flex flex-col md:flex-row justify-between items-center">
            <div className="mb-6 md:mb-0">
              <div className="text-2xl font-bold">
                codeer<span className="text-primary/70">.org</span>
              </div>
              <p className="text-sm text-muted-foreground mt-2">
                © 2025 codeer.org. All rights reserved.
              </p>
            </div>
            <div className="flex space-x-6">
              <a href="#" className="text-muted-foreground hover:text-primary transition-colors">
                Terms
              </a>
              <a href="#" className="text-muted-foreground hover:text-primary transition-colors">
                Privacy
              </a>
              <a href="#" className="text-muted-foreground hover:text-primary transition-colors">
                Contact
              </a>
              <a 
                href="https://github.com/sriox/webcompiler" 
                target="_blank" 
                rel="noopener noreferrer"
                className="text-muted-foreground hover:text-primary transition-colors"
              >
                GitHub
              </a>
            </div>
          </div>
        </div>
      </footer>
    </div>
  );
};

export default Index;
