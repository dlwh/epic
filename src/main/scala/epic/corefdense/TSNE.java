package epic.corefdense;

/**
 * Taken from https://code.google.com/p/wordsimilarity/
 * @author gdurrett
 *
 */
public class TSNE {
  
  public static double[][] tsne_p(double[][] p, String[] labels, int no_dims) {
    /*
            %TSNE_P Performs symmetric t-SNE on affinity matrix P
            %
            %   mappedX = tsne_p(P, labels, no_dims)
            %
            % The function performs symmetric t-SNE on pairwise similarity matrix P 
            % to create a low-dimensional map of no_dims dimensions (default = 2).
            % The matrix P is assumed to be symmetric, sum up to 1, and have zeros
            % on the diagonal.
            % The labels of the data are not used by t-SNE itself, however, they 
            % are used to color intermediate plots. Please provide an empty labels
            % matrix [] if you don't want to plot results during the optimization.
            % The low-dimensional data representation is returned in mappedX.
            %
            %
            % (C) Laurens van der Maaten, 2010
            % University of California, San Diego
     */

    boolean initial_solution = false;
    double realmin = 1e-100;

    // % First check whether we already have an initial solution
    //if numel(no_dims) > 1
    //    initial_solution = true;
    //    ydata = no_dims;
    //    no_dims = size(ydata, 2);
    //else
    //    initial_solution = false;
    //end

    // % Initialize some variables
    int n = p.length;                                    // % number of instances
    double momentum = 0.5;                               //      % initial momentum
    double final_momentum = 0.8;                         //      % value to which momentum is changed
    int mom_switch_iter = 250;                           //   % iteration at which momentum is changed
    int stop_lying_iter = 100;                           //   % iteration at which lying about P-values is stopped
    int max_iter = 1000;                                 //   % maximum number of iterations
    double epsilon = 500;                                //      % initial learning rate
    double min_gain = .01;                               //      % minimum gain for delta-bar-delta

    //% Make sure P-vals are set properly
    for(int i=0;i<p.length;i++) {
      p[i][i] = 0;
    }
    //P(1:n + 1:end) = 0;                                 % set diagonal to zero

    p = VectorTools.mult(VectorTools.getAdd(p, VectorTools.transpose(p)), .5);
    //P = 0.5 * (P + P');                                 % symmetrize P-values

    p = VectorTools.max(VectorTools.mult(p, 1 / VectorTools.sum(p)), realmin);
    //P = max(P ./ sum(P(:)), realmin);                   % make sure P-values sum to one

    //double c = VectorTools.sum(VectorTools.pointMult(p,VectorTools.log(p)));
    //const = sum(P(:) .* log(P(:)));                     % constant in KL divergence

    if(!initial_solution) {
      p = VectorTools.mult(p, 4);                                     // % lie about the P-vals to find better local minima
      //P = P * 4;
    }

    //% Initialize the solution
    //if(!initial_solution) {
    double[][] ydata = VectorTools.mult(VectorTools.randn(n, no_dims), .0001);
    /*double[][] ydata = {
        {0.0538,-0.0434},
        { 0.1834,0.0343},
       {-0.2259,0.3578},
        {0.0862,0.2769},
        { 0.0319,-0.1350},
       {-0.1308,0.3035}};
    ydata = VectorTools.mult(ydata, .001);*/
    //ydata = .0001 * randn(n, no_dims);
    //}

    double[][] y_incs  = new double[ydata.length][ydata[0].length];
    double[][] gains  = new double[ydata.length][ydata[0].length];
    for(int x=0;x<gains.length;x++) {
      for(int y=0;y<gains[x].length;y++) {
        gains[x][y] = 1;
      }   
    }

    //% Run the iterations
    for(int iter=0;iter<max_iter;iter++) {

      //% Compute joint probability that point i and j are neighbors

      double[][] sum_ydata = VectorTools.to2D(VectorTools.sumRow(VectorTools.pow(ydata,2)));
      //sum_ydata = sum(ydata .^ 2, 2);

      double num[][] = new double[1][1];
      //num = 1 ./ (1 + bsxfun(@plus, sum_ydata, bsxfun(@plus, sum_ydata', -2 * (ydata * ydata')))); % Student-t distribution
      double[][] inner = VectorTools.mult(VectorTools.mult(ydata, VectorTools.transpose(ydata)), -2);
      num = VectorTools.addAsRow(inner, VectorTools.transpose(sum_ydata));
      num = VectorTools.addAsCollumn(num, sum_ydata);
      num = VectorTools.pointDiv(VectorTools.ones(num.length,num[0].length), VectorTools.getAdd(VectorTools.ones(num.length,num[0].length),num));

      //num(1:n+1:end) = 0;                                                 % set diagonal to zero
      for(int i=0;i<p.length;i++) {
        num[i][i] = 0;
      }

      double[][] q = VectorTools.max(VectorTools.mult(num, 1 / VectorTools.sum(num)), realmin);
      //Q = max(num ./ sum(num(:)), realmin);                               % normalize to get probabilities

      //% Compute the gradients (faster implementation)

      double[][] l = VectorTools.pointMult(VectorTools.getSub(p, q),num);
      //L = (P - Q) .* num;

      inner = VectorTools.diag(VectorTools.sumRow(l));
      inner = VectorTools.getSub(inner, l);
      inner = VectorTools.mult(inner,4);
      double[][] y_grads = VectorTools.mult(inner,ydata);
      //y_grads = 4 * (diag(sum(L, 1)) - L) * ydata;

      //% Update the solution

      inner = VectorTools.pointMult(VectorTools.getAdd(gains, .2), VectorTools.notEqual(VectorTools.sign(y_grads), VectorTools.sign(y_incs)));
      double[][] inner2 = VectorTools.pointMult(VectorTools.mult(gains, .8), VectorTools.equal(VectorTools.sign(y_grads), VectorTools.sign(y_incs)));
      gains = VectorTools.getAdd(inner, inner2);
      //gains = (gains + .2) .* (sign(y_grads) ~= sign(y_incs)) ...         % note that the y_grads are actually -y_grads
      //      + (gains * .8) .* (sign(y_grads) == sign(y_incs));

      gains = VectorTools.max(gains, min_gain);
      //gains(gains < min_gain) = min_gain;

      y_incs = VectorTools.getSub(VectorTools.mult(y_incs, momentum), VectorTools.mult(VectorTools.pointMult(gains, y_grads), epsilon));
      //y_incs = momentum * y_incs - epsilon * (gains .* y_grads);

      ydata = VectorTools.getAdd(ydata, y_incs);
      //ydata = ydata + y_incs;

      ydata = VectorTools.subAsRow(ydata, VectorTools.transpose(VectorTools.mean(ydata)));
      //ydata = bsxfun(@minus, ydata, mean(ydata, 1));

      //% Update the momentum if necessary
      if(iter == mom_switch_iter) {
        momentum = final_momentum;
      }
      if(iter == stop_lying_iter && !initial_solution) {
        p = VectorTools.mult(p, 1 / 4.0);
        //P = P ./ 4;
      }

      /*
    % Print out progress
    if ~rem(iter, 10)
        cost = const - sum(P(:) .* log(Q(:)));
        disp(['Iteration ' num2str(iter) ': error is ' num2str(cost)]);
    end

    % Display scatter plot (maximally first three dimensions)
    if ~rem(iter, 10) && ~isempty(labels)
        if no_dims == 1
            scatter(ydata, ydata, 9, labels, 'filled');
        elseif no_dims == 2
            scatter(ydata(:,1), ydata(:,2), 9, labels, 'filled');
        else
            scatter3(ydata(:,1), ydata(:,2), ydata(:,3), 40, labels, 'filled');
                        end
                        axis tight
                        axis off
                        drawnow
                    end
                end
       */

    }

    return ydata;
  }
}
